namespace SudokuFromImage

module SudokuFromImage =
    open Emgu.CV
    open Emgu.CV.CvEnum
    open Emgu.CV.Structure
    open Emgu.CV.Util
    open Emgu.CV.OCR
    open System.Drawing
    open System
    
    type SudokuResult =
        | Grid of string
        | Error of string

    let private MatToArrayOfArrayOfInt (m : Mat) : int [][] =
        let arrayOfInt = Array.create (m.Cols * 4) 0
        m.CopyTo<int>(arrayOfInt)
        arrayOfInt |> Array.splitInto m.Cols 

    let private getParents (hierarchy : int [][]) : int List =
        hierarchy |> Array.map (fun indices -> indices.[3]) |> Array.filter ((<>) -1) |> Array.toList

    let private getParentsWithChilds  (hierarchy : int [][]) (parents : int List) : HashMap<int, int []> =
        let pXc = hierarchy |> Array.mapi (fun i indices -> 
                                               let p = indices.[3]
                                               if p <> -1 then p, i else -1, -1) 
                            |> Array.filter ((<>) (-1, -1))
        HashMap [for parent in parents -> parent, pXc |> Array.filter (fun pc -> match pc with | p, _ -> p = parent)
                                                      |> Array.map (fun pc -> match pc with | _, c -> c)]

    let private min (a: 'T []) : int = a |> Array.foldi (fun iMin i v -> if v < a.[iMin] then i else iMin) 0

    let private max (a: 'T []) : int = a |> Array.foldi (fun iMax i v -> if v > a.[iMax] then i else iMax) 0

    let private clockWisePoints (pts : PointF []) : VectorOfPointF =
        // top left, top right, bottom right, bottom left
        let sum = pts |> Array.map (fun p -> p.X + p.Y)
        let diff = pts |> Array.map (fun p -> p.X - p.Y)
        new VectorOfPointF([| pts.[min sum]; pts.[max diff]; pts.[max sum]; pts.[min diff] |])
    
    let sudokuFromImage (path : string)  : SudokuResult =
        try
            let image = CvInvoke.Imread(path, ImreadModes.ReducedGrayscale4)

            #if  DEBUG
            let temp = IO.Path.GetTempPath()
            CvInvoke.Imwrite(temp + "image.jpg", image) |> ignore
            #endif

            use imCanny = new UMat()
            CvInvoke.Canny(image, imCanny, 50.0, 150.0)

            #if  DEBUG
            CvInvoke.Imwrite(temp + "imCanny.jpg", imCanny) |> ignore
            #endif
            
            use imDilate = new UMat()
            CvInvoke.Dilate(imCanny, imDilate, null, Point(-1, -1), 1, BorderType.Default, MCvScalar(0.0, 0.0, 0.0))

            #if  DEBUG
            CvInvoke.Imwrite(temp + "imDilate.jpg", imDilate) |> ignore
            #endif
            
            use contours = new VectorOfVectorOfPoint()
            use hierarchy = new Mat()
            CvInvoke.FindContours(imDilate.Clone(), contours, hierarchy, RetrType.Ccomp, ChainApproxMethod.ChainApproxSimple)
             
            // Find the biggest square with 81 holes 
            let hData = MatToArrayOfArrayOfInt hierarchy
            let parents = getParents hData
            let parentsChilds = getParentsWithChilds hData parents
            let parents81 = [for parent in parents do if parentsChilds.[parent].Length >= 81 then yield parent]    // select contours with 81 holes or more
  
            if parents81.Length = 0 then
                Error "Pas de grille dans l'image"
            else
                let contoursA = contours.ToArrayOfArray()
                let parents81Corners = hashMap [for p in parents81 -> 
                                                    let corners = new VectorOfPoint()
                                                    CvInvoke.ApproxPolyDP(new VectorOfPoint(contoursA.[p]), corners, 5.0, true)
                                                    p, corners]  

                let parents81Square = parents81 |> List.filter (fun p -> parents81Corners.[p].Size = 4)   // select the contours that are rectangles
                
                if parents81Square.IsEmpty then
                    Error "Pas de grille dans l'image"
                else
                    let parents81Area = hashMap [for p in parents81 -> p, CvInvoke.ContourArea(contours.[p], false)]
                    let parent81 = parents81Square |> List.maxBy (fun p -> parents81Area.[p])       // select the rectangle with the greatest area
                    let corners = parents81Corners.[parent81]
                   
                    #if  DEBUG
                    use imCorners = image.Clone()
                    CvInvoke.DrawContours(imCorners, new VectorOfVectorOfPoint(corners), -1, MCvScalar(0.0, 0.0, 0.0), 2)
                    CvInvoke.Imwrite(temp + "imCorners.jpg", imCorners) |> ignore
                    #endif

                    // Correct the perspective
                    let rect = CvInvoke.BoundingRectangle(parents81Corners.[parent81])
                    let gridLen = Math.Max(rect.Height, rect.Width)     // grid side length
                    // The order of points is clockwise : top left, top right, bottom right, bottom left
                    use dstPoints = new VectorOfPointF([|   new PointF(0.0f, 0.0f);
                                                            new PointF(float32 gridLen - 1.0f, 0.0f); 
                                                            new PointF(float32 gridLen - 1.0f, float32 gridLen - 1.0f); 
                                                            new PointF(0.0f, float32 gridLen - 1.0f) |])
                    use srcPoints = [| for corner in corners.ToArray() -> new PointF(float32 corner.X, float32 corner.Y) |] |> clockWisePoints
                    use m = CvInvoke.GetPerspectiveTransform(srcPoints, dstPoints)
                    use imTrans = new UMat(gridLen, gridLen, DepthType.Cv8U, 1)
                    CvInvoke.WarpPerspective(image, imTrans, m, new Size(gridLen, gridLen))
                    
                    #if  DEBUG
                    CvInvoke.Imwrite(temp + "imTrans.jpg", imTrans) |> ignore
                    #endif

                    use imCanny2 = new UMat()
                    CvInvoke.Canny(imTrans, imCanny2, 50.0, 150.0)

                    #if  DEBUG
                    CvInvoke.Imwrite(temp + "imCanny2.jpg", imCanny2) |> ignore
                    #endif
            
                    let sideLen = float imCanny2.Size.Height / 9.0   // approximative length of square side 
                    let sideMid  = sideLen / 2.0    
                    //let sideMidSmall  = sideMid * 0.9     // to extract the digit without most of the grid lines 
                    
                    let squares = [for y in sideMid .. sideLen .. sideLen * 9.0 do 
                                        for x in sideMid .. sideLen .. sideLen * 9.0 -> 
                                            let square = new UMat(imCanny2, new Rectangle(new Point(int (x - sideMid), int (y - sideMid)), 
                                                                                           new Size(int sideLen, int sideLen))) 
                                            use contours = new VectorOfVectorOfPoint()
                                            CvInvoke.FindContours(square.Clone(), contours, null, RetrType.List, ChainApproxMethod.ChainApproxSimple)
                                            if contours.Size = 0 then
                                                None
                                            else
                                                let contoursA = contours.ToArrayOfArray()
                                                let digitContour = contoursA |> Array.maxBy (fun c -> c.Length)
                                                let digitRect = CvInvoke.BoundingRectangle(new VectorOfPoint(digitContour))
                                                Some (new UMat(square, digitRect))]

                    #if  DEBUG
                    squares |> Seq.iteri ( fun i s -> 
                                                match s with
                                                | None -> ()
                                                | Some m -> let fn = sprintf "ImDigits%02i.jpg" i
                                                            CvInvoke.Imwrite(temp + fn, m) |> ignore)
                    #endif
                   
                    use ocr = new Tesseract("", "eng", OcrEngineMode.TesseractOnly, "123456789")
                    let grid = [for square in squares ->
                                    match square with
                                    | None -> "."
                                    | Some m -> 
                                        ocr.Recognize(m)
                                        m.Dispose()
                                        match ocr.GetCharacters() with
                                        | ch when ch.Length = 0 -> "."
                                        | ch when ch.[0].Text = " " -> "."
                                        | ch -> ch.[0].Text]

                    Grid (String.concat "" grid)
        with
        | :?ArgumentException as ex -> Error ex.Message
        | _ -> reraise ()