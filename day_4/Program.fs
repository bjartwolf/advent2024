module Input =
    open System
    open System.IO
    open Xunit 
    open System.Text.RegularExpressions

    let readInit (filePath: string): string list = 
        File.ReadAllLines(filePath) |> Array.toList
    
    let createPertubations (input: string list): string seq = 
        let n = input.Length
        [for line in [0 .. (n - 1)] do
            // horiztonal lines
            yield input.[line]
            // vertical lines
            let vertical = [0 .. (n-1)] |> List.map (fun i -> input.[i].[line]) |> String.Concat
            yield vertical 
       ]

    let createPertubations2 (input: string list): string seq = 
        let n = input.Length
        [for line in [(-n+1) .. (n - 1)] do
            let stringLength = n - abs line
            if line = 0 then
                yield [0 .. stringLength - 1] |> List.map (fun i -> input.[i].[i]) |> String.Concat
            if line > 0 then
                yield [0 .. stringLength - 1] |> List.map (fun i -> input.[i].[i + line]) |> String.Concat
            if line < 0 then
                yield [0 .. stringLength - 1] |> List.map (fun i -> input.[i+(-line)].[i]) |> String.Concat
            if line = 0 then
                yield [0 .. stringLength - 1] |> List.map (fun i -> input.[stringLength - i - 1].[i]) |> String.Concat
            if line > 0 then
               yield [0 .. stringLength - 1] |> List.map (fun i -> input.[stringLength - i - 1 + line].[i + line]) |> String.Concat
            if line < 0 then
                yield [0 .. stringLength - 1] |> List.map (fun i -> input.[line + n - 1 - i].[i]) |> String.Concat
        ]

    let countOccurrences (input: string): int =
        let pattern = Regex.Escape("XMAS")
        Regex.Matches(input, pattern).Count

    let reverseString (input: string): string =
        new string(input.ToCharArray() |> Array.rev)

    let createAllPertubations (input: string list): string list =
        let pertubations1 = input |> createPertubations |> List.ofSeq
        let pertubations2 = input |> createPertubations2 |> List.ofSeq
        let rev1 = pertubations1 |> List.map reverseString 
        let rev2 = pertubations2 |> List.map reverseString 
        [pertubations1; pertubations2; rev1; rev2] |> List.concat


    let findOccurances (input: string list) : int =
        input |> List.map countOccurrences |> List.sum

    let isXmasCross (cross:string list): bool =
        let diag1 = [cross.[0][0]; cross.[1][1]; cross.[2][2]]  |> String.Concat
        let diag2 = [cross.[2][0]; cross.[1][1]; cross.[0][2]] |> String.Concat
        let diag1IsXmas = diag1 = "MAS" || diag1 |> reverseString = "MAS"
        let diag2IsXmas = diag2 = "MAS" || diag2 |> reverseString = "MAS"
        diag1IsXmas && diag2IsXmas 
        
    [<Fact>]
    let test3 () = 
        Assert.True( ["M_M";"_A_";"S_S"] |> isXmasCross)
        Assert.False( ["__S";"_A_";"M__"] |> isXmasCross)
        Assert.True(["M_S";"_A_";"M_S"] |> isXmasCross)


    let findCheckers (input: string list): ((string list) seq) =
        let n = input.Length
        [for line in [1 .. (n - 2)] do
            for col in [1 .. (n - 2)] do
                if (input.[line][col] = 'A') then
                    let lineAbove = input.[line - 1][col-1 ..col+1]
                    let lineAt = input.[line][col-1 ..col+1]
                    let lineBelow = input.[line + 1][col-1 ..col+1]
                    yield [lineAbove;lineAt;lineBelow]
        ]
        
    [<Fact>]
    let test4 () = 
        let input = readInit "input2.txt" 
        let checkers = input |> findCheckers
        let checkersWhichAreCross = checkers |> Seq.filter isXmasCross
        let count = checkersWhichAreCross |> Seq.length
        Assert.Equal(1873, count)


    // create all lines
    // there is all vertical lines
    // all horizontal lines
    // all downward diagonal lines
    // all upward diagonal lines
    
    // then all of them reversed
    // so vertical lines reverse
    // horizontal lines reversed
    // etc

    // then scan for the word in all the lines

    [<Fact>]
    let test2 () = 
//        let inputTest = ["abcde"; "fghij"; "klmno"; "pqrst"; "uvwxy"] //printf "%A" (inputTest |> createPertubations  |> List.ofSeq)
        //printf "%A" (inputTest |> createAllPertubations |> List.ofSeq)
        let input = readInit "input1.txt" 
        Assert.Equal(18, input |>createAllPertubations |> findOccurances) 
        let input2 = readInit "input2.txt" 
        Assert.Equal(2524, input2 |>createAllPertubations |> findOccurances) 
        //printf "%A" (input |> createPertubations  |> List.ofSeq)
        //Assert.Equal(1, input.Length) 

module Program = let [<EntryPoint>] main _ = 0