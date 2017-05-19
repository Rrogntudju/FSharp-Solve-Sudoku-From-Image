open SudokuFromImage.SudokuFromImage
open SudokuSolver.SudokuSolver

[<EntryPoint>]
let main argv = 
  
    let help = "\nssud - résoudre un sudoku à partir de son image\nusage: ssud [-h|--help|--aide] chemin de l'image\n"
    if argv.Length = 0 then
        printfn "%s" help
    else 
        if List.exists ((=) argv.[0]) ["--help"; "-h"; "--aide"] = true then
            printfn "%s" help
        else
            let grid  = sudokuFromImage argv.[0]
            match grid with
            | Grid g -> 
                    printfn "\n%s\n" g
                    solve g |> ignore
            | Error e -> printfn "%s\n" e
    0 