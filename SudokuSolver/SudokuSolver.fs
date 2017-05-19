namespace SudokuSolver

module SudokuSolver =

    // A translation of Peter Norvig’s Sudoku solver from Python to F#     http://www.norvig.com/sudoku.html
    open System.Collections.Generic

    let inline private isIn (l : 'a list) (i : 'a) = List.exists ((=) i) l   // exists is ~10X faster than contains
    let center (s : string) (w : int) =
        let len = s.Length
        if w > len then s.PadLeft(((w - len) / 2) + len).PadRight(w) else s

    let inline private (>>=) m f = Option.bind f m

    let rec private allSome (values :  Option<'a>) (fList : list<'a -> Option<'a>>) : Option<'a> =
        match fList with
            | [] -> values
            | f::fl when Option.isSome values -> allSome (values >>= f) fl
            | _ -> values

    let private firstSome (values :  Option<'a>) (fList : list<'a -> Option<'a>>) : Option<'a> =
        let rec firstSomeRec (values :  Option<'a>) (initVal : Option<'a>) (fList : list<'a -> Option<'a>>) : Option<'a> =
             match fList with
                | [] -> values
                | f::fl when Option.isNone values -> firstSomeRec (initVal >>= f) initVal fl
                | _ -> values

        firstSomeRec None values fList

    let private digits = ['1' .. '9']
    let private rows = ['A' .. 'I']
    let private cols = digits

    let private cross (rows : char list) (cols : char list) : string list = 
        [for ch in rows do for d in cols -> ch.ToString() + d.ToString()]  // a string is ~45% faster than a tuple as an HashMap key

    let private squares = cross rows cols
    let private unitlist = 
        [for d in cols -> cross rows [d]] @
        [for ch in rows -> cross [ch] cols] @
        [for r in [rows.[0..2]; rows.[3..5]; rows.[6..8]] do for c in [cols.[0..2]; cols.[3..5]; cols.[6..8]] -> cross r c]

    //  units is a dictionary where each square maps to the list of units that contain the square  
    let private units = HashMap [for s in squares -> s, unitlist |> List.filter (fun u -> s |> isIn u)]

    //  peers is a dictionary where each square s maps to the set of squares formed by the union of the squares in the units of s, but not s itself 
    let private peers = HashMap [for s in squares -> s, set(units.[s] |> List.concat |> List.filter ((<>) s))]

    let private display (values : HashMap<string, char list>) : Option<HashMap<string, char list>> =
        let pvalues = dict[for s in squares -> s, new string (Array.ofList values.[s])]  
        let width = ([for s in squares -> String.length pvalues.[s]] |> List.max) + 1
        let line = String.concat "+" [for _ in 1 .. 3 -> String.replicate (width * 3) "-"]
        for ch in rows do
            printfn "%s" (String.concat "" [for d in digits -> center pvalues.[ch.ToString() + d.ToString()] width + (if d |> isIn ['3'; '6'] then "|" else "")]) 
            if ch |> isIn ['C'; 'F'] then printfn "%s" line
        printfn "" 
        Some values

    let private grid_values (grid : string) : Option<HashMap<string, char list>> =
    //  Convert grid into a dict of (square, char list) with '0' or '.' for empties.
        let chars = [for ch in grid do if ch |> isIn digits || ch |> isIn ['.'; '0'] then yield [ch]]
        assert (chars.Length = 81)
        Some (HashMap (List.zip squares chars))

    let rec private assign (s : string) (d : char) (values : HashMap<string, char list>) : Option<HashMap<string, char list>> =
     (*  Assign a value d by eliminating all the other values (except d) from values[s] and propagate.  
        Return Some values, except return None if a contradiction is detected. *)   
        let other_values = values.[s] |> List.filter ((<>) d)
        [for d' in other_values -> eliminate s d'] |> allSome (Some values)
  
    and eliminate (s : string) (d : char) (values : HashMap<string, char list>) : Option<HashMap<string, char list>> =
  
            let rule1  (values : HashMap<string, char list>) : Option<HashMap<string, char list>> =
            //  (1) If a square s is reduced to one value d', then eliminate d' from the peers.
                match values.[s].Length with
                    | 0 -> None         // Contradiction: removed last value
                    | 1 -> let d' = values.[s].[0]
                           [for s' in peers.[s] -> eliminate s' d'] |> allSome (Some values)
                    | _ -> Some values

            let rule2 (values : HashMap<string, char list>) : Option<HashMap<string, char list>> =
            //  (2) If a unit u is reduced to only one place for a value d, then put it there.
                [for u in units.[s] -> fun (v : HashMap<string, char list>) ->
                    let dplaces = u |> List.filter (fun s' -> d |> isIn v.[s']) 
                    match dplaces.Length with
                        | 0 -> None  // Contradiction: no place for this value
                        | 1 -> assign dplaces.[0] d v   //  # d can only be in one place in unit; assign it there
                        | _ -> Some v
                ] |> allSome (Some values)

        //  Eliminate d from values.[s] and propagate. Return Some values, except return None if a contradiction is detected.        
            if not (d |> isIn values.[s]) then 
                Some values        // Already eliminated
            else
                let values' = values |> HashMap.add s (values.[s] |> List.filter ((<>) d))
                values' |> rule1 >>= rule2

    let private parse_grid (grid : string) : Option<HashMap<string, char list>> =
    //  Convert grid to Some dict of possible values, [square, digits], or return None if a contradiction is detected. 
        let assignGrid (gvalues : HashMap<string, char list>)  =
            let values = HashMap (squares |> List.map (fun s -> s, digits))
            [for s in squares do for d in gvalues.[s] do if d |> isIn digits then yield assign s d] |> allSome (Some values)
        grid_values grid >>= assignGrid

    let rec private search (values : HashMap<string, char list>) : Option<HashMap<string, char list>> =
    //  Using depth-first search and propagation, try all possible values.
        if seq {for s in squares -> values.[s].Length = 1} |> Seq.forall (id) then
            Some values   //    Solved!
        else
    //      Choose the unfilled square s with the fewest possibilities
            let _, s = seq {for s in squares do if values.[s].Length > 1 then yield values.[s].Length, s} |> Seq.min
            [for d in values.[s] -> fun v -> assign s d v >>= search] |> firstSome (Some values) 
 
    let private solved (values : HashMap<string, char list>) : Option<HashMap<string, char list>> =
    //  A puzzle is solved if each unit is a permutation of the digits 1 to 9
        let isUnitSolved u = Set (seq {for s in u -> values.[s]}) = Set (seq {for d in digits -> [d]})
        match seq {for u in unitlist -> isUnitSolved u} |> Seq.forall (id) with
            | true -> Some values
            | false ->  printfn "Le sudoku n'a pas été résolu!\n"
                        None
   
    let solve (grid : string) : bool = 
        grid |> grid_values >>= display |> ignore
        let result = grid |> parse_grid >>= search >>= solved >>= display
        match result with
        | Some _ -> true
        | None -> false
   


