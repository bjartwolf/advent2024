module Input =
    open System
    open System.IO
    open Xunit 

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
            printfn "%A %A" stringLength line
            (*
            if line = 0 then
                yield [0 .. stringLength - 1] |> List.map (fun i -> input.[i].[i]) |> String.Concat
            if line > 0 then
                yield [0 .. stringLength - 1] |> List.map (fun i -> input.[i].[i + line]) |> String.Concat
            if line < 0 then
                yield [0 .. stringLength - 1] |> List.map (fun i -> input.[i+(-line)].[i]) |> String.Concat
*)
            if line = 0 then
                yield [0 .. stringLength - 1] |> List.map (fun i -> input.[stringLength - i - 1].[i]) |> String.Concat
            if line > 0 then
               yield [0 .. stringLength - 1] |> List.map (fun i -> input.[stringLength - i - 1 + line].[i + line]) |> String.Concat
            if line < 0 then
                //printfn "%A" (line )
                yield [0 .. stringLength - 1] |> List.map (fun i -> input.[line + n - 1 - i].[i]) |> String.Concat
//            let dVertical = [0 .. stringLength - 1] 
 //                                   |> List.map (fun i -> input.[i].[i])  
  //                                  |> String.Concat
   //         yield dVertical
            //yield line |> string
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
        let inputTest = ["abcde"; 
                         "fghij"; 
                         "klmno"; 
                         "pqrst"; 
                         "uvwxy"]
        //printf "%A" (inputTest |> createPertubations  |> List.ofSeq)
        printf "%A" (inputTest |> createPertubations2  |> List.ofSeq)
//        let input = readInit "input1.txt" 
        //printf "%A" (input |> createPertubations  |> List.ofSeq)
        //Assert.Equal(1, input.Length) 

module Program = let [<EntryPoint>] main _ = 0