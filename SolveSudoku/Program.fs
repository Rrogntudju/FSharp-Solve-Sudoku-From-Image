open SudokuFromImage.SudokuFromImage

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let grid  = sudokuFromImage "C:\Users\Spock\Downloads\sudoku3.jpg"
    match grid with
        | Grid d -> printfn "%s" d
        | Error e -> printfn "%s" e

    0 // retourne du code de sortie entier