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

    let private getParentsWithChilds  (hierarchy : int [][]) : (int * int) [] =
        hierarchy |> Array.mapi (fun i indices -> 
                        let p = indices.[3]
                        if p <> -1 then p, i else -1, -1) 
                  |> Array.filter ((<>) (-1, -1))

           
    let sudokuFromImage (path : string)  : SudokuResult =
        try
            let image = CvInvoke.Imread(path, ImreadModes.ReducedGrayscale4)
        
            let imCanny = new Mat()
            CvInvoke.Canny(image, imCanny, 50.0, 150.0)
            
            let imDilate = new Mat()
            CvInvoke.Dilate(imCanny, imDilate, null, Point(-1, -1), 1, BorderType.Default, MCvScalar(0.0, 0.0, 0.0))
            
            let contours = new VectorOfVectorOfPoint()

            //let lines = CvInvoke.HoughLinesP(imDilate, 1.0, Math.PI/180.0, 50, 50.0, 10.0 );
    
            //let imLines = image.Clone()
            //for line : LineSegment2D in lines do
            //    CvInvoke.Line(imLines, line.P1, line.P2, MCvScalar(0.0, 0.0, 255.0), 3, LineType.AntiAlias);
            
            let hierarchie = new Mat()
            CvInvoke.FindContours(imDilate.Clone(), contours, hierarchie, RetrType.Ccomp, ChainApproxMethod.ChainApproxSimple)
            
            // CvInvoke.DrawContours(image, contours, -1, MCvScalar(0.0, 0.0, 0.0), 2)

            // Find the biggest contour with 81 holes
            let hData = MatToArrayOfArrayOfInt hierarchie
            let parentsChilds = getParentsWithChilds hData
            let parents = parentsChilds |> Array.map (fun pc -> match pc with | p, _ -> p) |> Array.distinct
            let parents81Childs = [for p in parents do if (parentsChilds |> Array.filter (fun pc -> match pc with | p2, _ -> p2 = p) |> Array.length) = 81 then yield p]
  
            
            let ca = contours.ToArrayOfArray()
            let sud = Array.sub ca parents81Childs.Head 1 
            CvInvoke.DrawContours(image, new VectorOfVectorOfPoint(sud), -1, MCvScalar(0.0, 0.0, 0.0), 2)
            
            #if  DEBUG
            let temp = IO.Path.GetTempPath()
            CvInvoke.Imwrite(temp + "image.jpg", image) |> ignore
            CvInvoke.Imwrite(temp + "canny.jpg", imCanny) |> ignore
            CvInvoke.Imwrite(temp + "dilate.jpg", imDilate) |> ignore
            #endif
            
            Grid ""

        with
            | :?ArgumentException as ex -> Error ex.Message
            |_ -> reraise()
            
