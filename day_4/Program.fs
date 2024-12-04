module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): string list = 
        File.ReadAllLines(filePath) |> Array.toList
    
    let createPertubations (wordLength: int) (input: string list): string seq = 
        let n = input.Length
        [for line in [0 .. (n - 1)] do
            // horiztonal lines
            yield input.[line]

            // vertical lines
            let vertical = System.String.Concat([0 .. (n-1)] |> List.map (fun i -> input.[i].[line]) )
            yield vertical 
        ]


        // horizontal lines


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
        let inputTest = ["abcde"; "fghij"; "klmno"; "pqrst"; "uvwxy"]
        printf "%A" (inputTest |> createPertubations 4 |> List.ofSeq)
//        let input = readInit "input1.txt" 
        //printf "%A" (input |> createPertubations 4 |> List.ofSeq)
        //Assert.Equal(1, input.Length) 

module Program = let [<EntryPoint>] main _ = 0