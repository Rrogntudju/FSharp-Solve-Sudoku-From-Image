﻿namespace SudokuFromImage

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
             
            // Find the biggest contour with 81 holes 
            let hData = MatToArrayOfArrayOfInt hierarchy
            let parents = getParents hData
            let parentsChilds = getParentsWithChilds hData parents
            let parents81 = [for parent in parents do if parentsChilds.[parent].Length >= 81 then yield parent]
  
            if parents81.Length = 0 then
                Error "Pas de grille dans l'image"
            else
                let parents81Area = Collections.Map [for p in parents81 -> CvInvoke.ContourArea(contours.[p], false), p]
                let parent81 = parents81Area.[parents81Area |> Map.keys |> Set.maxElement]      // biggest contour with 81 holes (or more)
           
                // Get the 4 corners of the grid
                let contoursA = contours.ToArrayOfArray()
                use corners = new VectorOfPoint()
                CvInvoke.ApproxPolyDP(new VectorOfPoint(contoursA.[parent81]), corners, 5.0, true)

                #if  DEBUG
                use imCorners = image.Clone()
                CvInvoke.DrawContours(imCorners, new VectorOfVectorOfPoint(corners), -1, MCvScalar(0.0, 0.0, 0.0), 2)
                CvInvoke.Imwrite(temp + "imCorners.jpg", imCorners) |> ignore
                #endif

                if corners.Size <> 4 then
                    Error (sprintf "La grille a %i coins..." corners.Size)
                else
                    // Correct the perspective
                    let rect = CvInvoke.BoundingRectangle(corners)
                    let gridLen = Math.Max(rect.Height, rect.Width)     // grid side length
                    // The order of points is clockwise : top left, top right, bottom right, bottom left
                    use dstPoints = new VectorOfPointF([| new PointF(0.0f, 0.0f);
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

                    //// convert grayscale image to binary image
                    let imThresh = new UMat()
                    CvInvoke.AdaptiveThreshold(imTrans, imThresh, 255.0, AdaptiveThresholdType.MeanC, ThresholdType.Binary, 9, 9.0)
                                                  
                    #if  DEBUG
                    CvInvoke.Imwrite(temp + "imThresh.jpg", imThresh) |> ignore
                    #endif

                    //let imBlur = new Mat()
                    //CvInvoke.Blur(imThresh, imBlur, new Size(3, 3), new Point (-1, -1))
                                                  
                    //#if  DEBUG
                    //CvInvoke.Imwrite(temp + "imBlur.jpg", imBlur) |> ignore
                    //#endif 
                    //let sLen = float imThresh.Size.Height / 9.0       // length of square side 
                    //let sMid  = sLen / 2.0                           // middle of the square side
                    //let sMidSmall  = sMid * 0.75     // to extract a smaller square centered in the middle of the bigger square. The goal is to extract the digit without the grid lines 
                    //let squares = seq {for y in sMid .. sLen .. sLen * 9.0 do 
                    //                        for x in sMid .. sLen .. sLen * 9.0 -> 
                    //                             new UMat(imThresh, new Rectangle(new Point(int (x - sMidSmall), int (y - sMidSmall)), 
                    //                                                             new Size(int (sMidSmall * 2.0), int (sMidSmall * 2.0)))) 

                    let sideLen = float imThresh.Size.Height / 9.0       // approximative length of square side 
                    let sideMid  = sideLen / 2.0    
                    let sideMidSmall  = sideMid * 0.9     // to extract the digit without most of the grid lines 
                    
                    let squares = seq {for y in sideMid .. sideLen .. sideLen * 9.0 do 
                                            for x in sideMid .. sideLen .. sideLen * 9.0 -> 
                                                use square = new UMat(imThresh, new Rectangle(new Point(int (x - sideMidSmall), int (y - sideMidSmall)), 
                                                                                              new Size(int (sideMidSmall * 2.0), int (sideMidSmall * 2.0)))) 
                                                use contours = new VectorOfVectorOfPoint()
                                                CvInvoke.FindContours(square.Clone(), contours, null, RetrType.List, ChainApproxMethod.ChainApproxSimple)
                                                let contoursA = contours.ToArrayOfArray()
                                                let contoursArea = Collections.Map [for c in contoursA -> c.Length, c]
                                                let squareContour = contoursArea.[contoursArea |> Map.keys |> Set.maxElement]   // extract the most «convoluted» contour
                                                let squareRect = CvInvoke.BoundingRectangle(new VectorOfPoint(squareContour))
                                                new UMat(square, squareRect)

                                       }

                    #if  DEBUG
                    squares |> Seq.iteri (fun i s -> let fn = sprintf "ImDigits%02i.jpg" i
                                                     CvInvoke.Imwrite(temp + fn, s) |> ignore
                                                     s.Dispose())
                    #endif
                     
                    //use ocr = new Tesseract("", "eng", OcrEngineMode.TesseractOnly)
                    //let grid = [for square in squares ->
                    //               ocr.Recognize(square)
                    //               let digits = ocr.GetCharacters() |> Array.filter (fun (ch : Tesseract.Character) -> "123456789".Contains(ch.Text))
                    //               square.Dispose()
                    //               if digits.Length = 0 then 
                    //                  "."
                    //               else 
                    //                  digits.[0].Text
                    //            ]
                                
                    use ocr = new Tesseract("", "eng", OcrEngineMode.TesseractOnly, "123456789")
                    let grid = [for square in squares ->
                                   ocr.Recognize(square)
                                   let digits = ocr.GetCharacters()
                                   square.Dispose()
                                   if digits.Length = 0 then 
                                      "."
                                   else 
                                      digits.[0].Text
                                ]
                    Grid (String.concat "" grid)
        with
            | :?ArgumentException as ex -> Error ex.Message
            |_ -> reraise ()