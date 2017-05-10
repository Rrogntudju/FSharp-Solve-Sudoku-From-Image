open SudokuFromImage.SudokuFromImage
open SudokuSolver.SudokuSolver

[<EntryPoint>]
let main argv = 
  
    let help = "ssud - résoudre un sudoku à partir de son image\nusage: ssud [-h|--help|--aide] chemin de l'image"
    if argv.Length = 0 then
        printfn "%s" help
    else 
        if List.exists ((=) argv.[0]) ["--help"; "-h"; "--aide"] = true then
            printfn "%s" help
        else
            let grid  = sudokuFromImage argv.[0]
            match grid with
                | Grid g -> solve g
                | Error e -> printfn "%s" e
    0 