open SudokuFromImage.SudokuFromImage

[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    
    #if DEBUG
    let results = [|"7....4...8..6.9...93...7.14.......7..25.....3...182..9..6....32348...7......6.54.";
                    ".8.1..7....3.5........47.2981..3.....5............126...4....17.....6...9..78....";
                    ".7..6..8.4.......6..57.82....24.61..7...1...5.9..3..6.86...7.54.........52.9.4.31";
                    ".4.8.....1..9..7....7.3.....6.....9...514...8.....942.3.16.2.......51..69.....2.1";
                    "9..7..4...32...8.......5..6...82.56..8..7..1...7.....464.218.......4.25.8........";
                    "2.3..4......9..8.119.2...4.........6.4.87.2....7.2.3.4..2.5....9.6..8...7.8.1.43."|]

    let sudokus = [|"C:\Users\Spock\Downloads\sudoku1.jpg";
                    "C:\Users\Spock\Downloads\sudoku2.jpg";
                    "C:\Users\Spock\Downloads\sudoku3.jpg";
                    "C:\Users\Spock\Downloads\sudoku4.jpg";
                    "C:\Users\Spock\Downloads\sudoku5.jpg"; 
                    "C:\Users\Spock\Downloads\sudoku6.jpg";|]
    
    sudokus |> Array.iteri (fun i s -> 
                                let grid  = sudokuFromImage s
                                match grid with
                                    | Grid g when g = results.[i] -> printfn "Test %i : OK" (i + 1)
                                    | Grid g -> printfn "Test %i : Échec" (i + 1)
                                    | Error e -> printfn "Test %i : %s" (i + 1) e)
    #endif

    //let grid  = sudokuFromImage "C:\Users\Spock\Downloads\sudoku6.jpg"
    //match grid with
    //    | Grid d -> printfn "%s" d
    //    | Error e -> printfn "%s" e

    0 