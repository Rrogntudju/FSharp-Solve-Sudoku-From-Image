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
                    "2.3..4......9..8.119.2...4.........6.4.87.2....7.2.3.4..2.5....9.6..8...7.8.1.43.";
                    "...........714..69..98.32...........6.29.84.33.....8....8.7...45......2....5.13..";
                    ".61....5.5.7.3.....9...72..9.4.2......53.14.8....9....7..14.9..6.....5.21....937.";
                    "....4.7.8.....7.5..7...2..18...2.6.9.5..813...1.4..8...6973.4..32.6..5...........";
                    "....4.7.8.....7.5..7...2..18...2.6.9.5..813...1.4..8...6973.4..32.6..5...........";
                    "8..715..4..53.67..3.64.89.1.6..5..4....8.7....5..4..9.6.95.34.2..49.25..5..164..9";
                    ".53.....1.278........5..89.....7..1..391....57....82.....4.1...56.......2..9..5.8";
                    "2.4.7..3589..14.7..578....418.3..5.2.2.....4.4.5..9.879....536..6.48..5957..9.4.1"|]

    let sudokus = [|"C:\Users\Spock\Downloads\sudoku1.jpg";
                    "C:\Users\Spock\Downloads\sudoku2.jpg";
                    "C:\Users\Spock\Downloads\sudoku3.jpg";
                    "C:\Users\Spock\Downloads\sudoku4.jpg";
                    "C:\Users\Spock\Downloads\sudoku5.jpg"; 
                    "C:\Users\Spock\Downloads\sudoku6.jpg";
                    "C:\Users\Spock\Downloads\sudoku7.jpg";
                    "C:\Users\Spock\Downloads\sudoku8.jpg";
                    "C:\Users\Spock\Downloads\sudoku9.jpg";
                    "C:\Users\Spock\Downloads\sudoku10.jpg";
                    "C:\Users\Spock\Downloads\sudoku11.jpg";
                    "C:\Users\Spock\Downloads\sudoku12.jpg";
                    "C:\Users\Spock\Downloads\sudoku13.jpg"|]
    
    sudokus |> Array.iteri (fun i s -> 
                                let grid  = sudokuFromImage s
                                match grid with
                                    | Grid g when g = results.[i] -> printfn "Test %i : OK" (i + 1)
                                    | Grid g -> printfn "Test %i : Échec" (i + 1)
                                    | Error e -> printfn "Test %i : %s" (i + 1) e)
    #endif

    let grid  = sudokuFromImage "C:\Users\Spock\Downloads\sudoku13.jpg"
    match grid with
        | Grid d -> printfn "%s" d
        | Error e -> printfn "%s" e

    0 