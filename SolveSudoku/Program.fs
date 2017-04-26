open SudokuFromImage.SudokuFromImage

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let grid  = sudokuFromImage "C:\Users\Spock\Downloads\sudoku4.jpg"
    match grid with
        | Grid d -> printfn "%s" d
        | Error e -> printfn "%s" e

    0 