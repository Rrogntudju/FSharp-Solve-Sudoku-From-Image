# Introduction
A console application in F# to solve a sudoku puzzle from the 9x9 grid image. Uses Emgu (OpenCV).

# Download the dependencies
1.	Install the x64 Cuda version of Emgu 3.2 : https://sourceforge.net/projects/emgucv/files/emgucv/3.2/libemgucv-windesktop_x64-cuda-3.2.0.2682.exe/download
2.	Download the english trained data file for Tesseract OCR 4 : https://github.com/tesseract-ocr/tessdata/blob/master/eng.traineddata

# Build and Test
The project SudokuFromImage needs a reference to Emgu.CV.World.dll (to be found in the Emgu installation directory). 
The project SudokuSolver needs a reference to ExtCore (a NuGet package).
The project SolveSudoku builds the console application ssud.exe.
The project Tests builds a test of 16 grid images.

In order to successfully run the tests or ssud.exe, you need to :
    Copy the x64 folder from the Emgu installation in the executable folder
    Create a tessdata folder containing the eng.traineddata file in the executable folder