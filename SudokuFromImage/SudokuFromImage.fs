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

    let sudokuFromImage (path : string)  : SudokuResult =
        try
            let image = CvInvoke.Imread(path, ImreadModes.ReducedGrayscale4)
        
          //  let mutable imThresh = new Mat()
          //  CvInvoke.Threshold(image, imThresh, 150.0, 255.0, ThresholdType.Binary) |> ignore
    
            //let mutable imBlur = new Mat()
            //CvInvoke.Blur(image, imBlur, Size(3,3), Point(-1, -1));

            let imCanny = new Mat()
            CvInvoke.Canny(image, imCanny, 50.0, 150.0)
            
            let imDilate = new Mat()
            CvInvoke.Dilate(imCanny, imDilate, new Mat(), Point(-1, -1), 1, BorderType.Default, MCvScalar(0.0, 0.0, 0.0))

            let lines = CvInvoke.HoughLinesP(imDilate, 1.0, Math.PI/180.0, 50, 50.0, 10.0 );
    
            let imLines = image.Clone()
            for line : LineSegment2D in lines do
                CvInvoke.Line(imLines, line.P1, line.P2, MCvScalar(0.0, 0.0, 255.0), 3, LineType.AntiAlias);
  
            #if  DEBUG
            let temp = IO.Path.GetTempPath()
            CvInvoke.Imwrite(temp + "image.jpg", image) |> ignore
            CvInvoke.Imwrite(temp + "canny.jpg", imCanny) |> ignore
            CvInvoke.Imwrite(temp + "dilate.jpg", imDilate) |> ignore
            CvInvoke.Imwrite(temp + "lines.jpg", imLines) |> ignore
            #endif
            
            Grid ""

        with
            | :?ArgumentException as ex -> Error ex.Message
            |_ -> reraise()
            
