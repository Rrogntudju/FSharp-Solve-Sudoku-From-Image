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
        let pXc = [| for h in (hierarchy |> Array.indexed) do   let c, indices = h 
                                                                let p = indices.[3]
                                                                if p <> -1 then yield p, c |]
        
        HashMap [for parent in parents -> parent, pXc |> Array.filter (fun pc -> match pc with | p, _ -> p = parent)
                                                      |> Array.map (fun pc -> match pc with | _, c -> c)]

    let private min (a: 'T []) : int = a |> Array.foldi (fun iMin i v -> if v < a.[iMin] then i else iMin) 0

    let private max (a: 'T []) : int = a |> Array.foldi (fun iMax i v -> if v > a.[iMax] then i else iMax) 0

    let private clockWisePoints (pts : PointF []) : VectorOfPointF =
        // top left, top right, bottom right, bottom left
        let sum = pts |> Array.map (fun p -> p.X + p.Y)
        let diff = pts |> Array.map (fun p -> p.X - p.Y)
        new VectorOfPointF([| pts.[min sum]; pts.[max diff]; pts.[max sum]; pts.[min diff] |])

    let private dist (p1 : Point) (p2 : Point) : float =
            Math.Sqrt(Math.Pow(float p1.X - float p2.X, 2.0) + Math.Pow(float p1.Y - float p2.Y, 2.0))
    
    let sudokuFromImage (path : string)  : SudokuResult =
        try
            use imageFullSize = CvInvoke.Imread(path, ImreadModes.Grayscale)

            let factor = Math.Sqrt(float (imageFullSize.Height * imageFullSize.Width) / 750000.0)
            use image =
                if factor < 1.4 then
                    imageFullSize
                else 
                    let image = new Mat()
                    CvInvoke.Resize(imageFullSize, image, new Size(0, 0), 1.0 / factor, 1.0 / factor, Inter.Area)
                    image

            #if  DEBUG
            let temp = IO.Path.GetTempPath()
            CvInvoke.Imwrite(temp + "image.jpg", image) |> ignore
            #endif

            use imCanny = new UMat()
            CvInvoke.Canny(image, imCanny, 50.0, 150.0)

            #if  DEBUG
            CvInvoke.Imwrite(temp + "imCanny.jpg", imCanny) |> ignore
            #endif
    
            let dilateAndFindContours (iter : int) : (VectorOfVectorOfPoint * int list) =
                use imDilate = new UMat()
                CvInvoke.Dilate(imCanny, imDilate, null, Point(-1, -1), iter, BorderType.Default, MCvScalar(0.0, 0.0, 0.0))

                #if  DEBUG
                CvInvoke.Imwrite(temp + "imDilate.jpg", imDilate) |> ignore
                #endif
            
                let contours = new VectorOfVectorOfPoint()
                // Emgu wants a Mat instead of a VectorOfVectorOfInt for the hierarchy...
                use hierarchy = new Mat()
                CvInvoke.FindContours(imDilate, contours, hierarchy, RetrType.Ccomp, ChainApproxMethod.ChainApproxSimple)
             
                // Find the biggest square with 81 holes 
                let hData = MatToArrayOfArrayOfInt hierarchy
                let parents = getParents hData
                let parentsChilds = getParentsWithChilds hData parents
                let parents81 = [for parent in parents do if parentsChilds.[parent].Length > 81 then yield parent]    // select contours with 81 holes or more
                contours, parents81

            let findGrid (iter : int list) : (VectorOfVectorOfPoint * int list) =
                let rec findGridRec (result : VectorOfVectorOfPoint * int list) (iter : int list) : (VectorOfVectorOfPoint * int list) =
                    match iter with
                    | _ when (snd result).Length > 0 -> result
                    | [] -> result
                    | p::pl -> findGridRec (dilateAndFindContours p) pl 

                iter |> findGridRec (new VectorOfVectorOfPoint(), [])

            // Try to find the grid by increasing the value of dilate iteration parameter (to fuse the 2 edges of a large grid line)
            let contours, parents81 = [1..4] |> findGrid 

            if parents81.Length = 0 then
                Error "Pas de grille dans l'image"
            else
                let contoursA = contours.ToArrayOfArray()
                let parents81Corners = hashMap [for p in parents81 -> 
                                                    let corners = new VectorOfPoint()
                                                    CvInvoke.ApproxPolyDP(new VectorOfPoint(contoursA.[p]), corners, 10.0, true)  // magic! magic!
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

                    // Flatten the perspective
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
                    
                    use imThresh = new UMat()
                    CvInvoke.AdaptiveThreshold(imTrans, imThresh, 255.0, AdaptiveThresholdType.GaussianC, ThresholdType.Binary, 19, 11.0)   // magic! magic!
                    
                    #if  DEBUG
                    CvInvoke.Imwrite(temp + "imThresh.jpg", imThresh) |> ignore
                    #endif
            
                    let gridLen = float imThresh.Size.Height
                    let squareLen = float gridLen / 9.0   // approximative length of square side 
                    let squareArea = Math.Pow(squareLen, 2.0)
                    let squareCenter = new Point(int (squareLen / 2.0), int (squareLen / 2.0))
                    let squareSize = new Size(int squareLen, int squareLen)

                    let squares = [for y in 0.0 .. squareLen .. gridLen - squareLen do 
                                        for x in 0.0 .. squareLen .. gridLen - squareLen -> 
                                            new UMat(imThresh, new Rectangle(new Point(int x, int y), squareSize))]
                    
                    #if  DEBUG
                    squares |> List.iteri ( fun i square -> 
                                                let fn = temp + sprintf "ImSquares%02i.jpg" i
                                                CvInvoke.Imwrite(fn, square) |> ignore)
                    #endif

                    let digits = squares 
                                    |> List.map (fun square ->
                                                use contours = new VectorOfVectorOfPoint()
                                                CvInvoke.FindContours(square.Clone(), contours, null, RetrType.List, ChainApproxMethod.ChainApproxSimple)
                                                if contours.Size = 0 then
                                                    None
                                                else
                                                    let rects = [for contour in contours.ToArrayOfArray() ->  CvInvoke.BoundingRectangle(new VectorOfPoint(contour))]
                                                    let digits = rects 
                                                                    |> List.map (fun rect -> 
                                                                                // The area of the digit bounding rectangle should be «smaller» than the area of the square
                                                                                let digitRatio = (float rect.Height * float rect.Width) / squareArea
                                                                                if digitRatio > 0.70 || digitRatio < 0.08 then  
                                                                                    None
                                                                                else
                                                                                    // The center of the digit bounding rectangle should be «near» the center of the square
                                                                                    let rectCenter = new Point(rect.X + (rect.Width / 2), rect.Y + (rect.Height / 2))
                                                                                    if (dist rectCenter squareCenter) / squareLen > 0.30 then
                                                                                        None
                                                                                    else
                                                                                        // The digit image should not be «too empty» 
                                                                                        let digit = new UMat(square, rect)
                                                                                        let whitePixels = new VectorOfPoint()
                                                                                        CvInvoke.FindNonZero(digit, whitePixels)
                                                                                        if float whitePixels.Size / float (digit.Size.Height * digit.Size.Width) > 0.8 then
                                                                                            digit.Dispose()
                                                                                            None
                                                                                        else
                                                                                            Some digit
                                                                                )

                                                    if digits |> List.forall (Option.isNone) = true then
                                                        None
                                                    else
                                                        digits |> List.find (Option.isSome)
                                                )
                                                
                    #if  DEBUG
                    digits |> List.iteri ( fun i digit -> 
                                                let fn = temp + sprintf "ImDigits%02i.jpg" i
                                                IO.File.Delete(fn)
                                                match digit with
                                                | None -> ()
                                                | Some m -> CvInvoke.Imwrite(fn, m) |> ignore)                                                   
                    #endif
                   
                    // Tesseract is finicky
                    use ocr = new Tesseract("", "eng", OcrEngineMode.TesseractOnly, "123456789")
                    let grid = [for digit in digits ->
                                    match digit with
                                    | None -> "."
                                    | Some m -> 
                                        ocr.SetImage(m)
                                        ocr.Recognize() |> ignore
                                        m.Dispose()
                                        match ocr.GetCharacters() with
                                        | ch when ch.Length = 0 -> "."
                                        | ch when ch.[0].Text = " " -> "."
                                        | ch -> ch.[0].Text
                                ]
                    
                    if (digits |> List.filter (Option.isSome)).Length <> (grid |> List.filter ((<>) ".")).Length then
                        Error (sprintf "Tesseract n'a pas pu lire au moins un des nombres\n%s" (String.concat "" grid))
                    else
                        Grid (String.concat "" grid)
        with
        | :?ArgumentException as ex -> Error ex.Message
        | _ -> reraise ()