namespace SudokuFromImage

module SudokuFromImage =
    open Emgu.CV
    open Emgu.CV.CvEnum
    open Emgu.CV.Structure
    open Emgu.CV.Util
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

            let imCanny = new Mat()
            CvInvoke.Canny(image, imCanny, 50.0, 150.0)

            #if  DEBUG
            CvInvoke.Imwrite(temp + "imCanny.jpg", imCanny) |> ignore
            #endif
            
            let imDilate = new Mat()
            CvInvoke.Dilate(imCanny, imDilate, null, Point(-1, -1), 1, BorderType.Default, MCvScalar(0.0, 0.0, 0.0))

            #if  DEBUG
            CvInvoke.Imwrite(temp + "imDilate.jpg", imDilate) |> ignore
            #endif
            
            let contours = new VectorOfVectorOfPoint()
            let hierarchy = new Mat()
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
                let ca = contours.ToArrayOfArray()
                let corners = new VectorOfPoint()
                CvInvoke.ApproxPolyDP(new  VectorOfPoint(ca.[parent81]), corners, 5.0, true)
   //             CvInvoke.DrawContours(image, new VectorOfVectorOfPoint(corners), -1, MCvScalar(0.0, 0.0, 0.0), 2)
                if corners.Size <> 4 then
                    Error (sprintf "La grille a %i coins..." corners.Size)
                else
                    // Correct the perspective
                    let rect = CvInvoke.BoundingRectangle(corners)
                    let len = Math.Max(rect.Height, rect.Width)
                    // The order of points is clockwise : top left, top right, bottom right, bottom left
                    let dstPoints = new VectorOfPointF([| new PointF(0.0f, 0.0f);
                                                          new PointF(float32 len, 0.0f); 
                                                          new PointF(float32 len, float32 len); 
                                                          new PointF(0.0f, float32 len) |])
                    let srcPoints = [| for corner in corners.ToArray() -> new PointF(float32 corner.X, float32 corner.Y) |] |> clockWisePoints
                    let m = CvInvoke.GetPerspectiveTransform(srcPoints, dstPoints)
                    let imTrans = new Mat(len, len, DepthType.Cv8U, 1)
                    CvInvoke.WarpPerspective(image, imTrans, m, new Size(len, len))
                    
                    #if  DEBUG
                    CvInvoke.Imwrite(temp + "imTrans.jpg", imTrans) |> ignore
                    #endif
                   
                    Grid ""
        with
            | :?ArgumentException as ex -> Error ex.Message
            |_ -> reraise ()