module Input =
    open System
    open System.IO
    open Xunit 

    let example = "12345"
    let readInit (filePath: string): string = File.ReadAllText(filePath) 

    let parseInput (input: string): int list =
        input |> Seq.toList|> Seq.map (fun c -> int c - int '0' ) |> Seq.toList


    let expandPattern (input: int list) : (int option) list = 
        let rec expand (input: int list) (idNr: int) = 
            match input with
            | [] -> [] 
            | file::block::xs -> List.replicate file (Some idNr) @  (List.replicate block None) @ expand xs (idNr + 1)
            | file::[] -> List.replicate file (Some idNr) 
        expand input 0

    let printPattern (input: int option list): string = 
        input |> List.map (fun x -> match x with | Some x -> string x | None -> ".") |> String.concat ""

    let printCompact (input: int list): string = 
        input |> List.map (fun x -> match x with | x -> string x) |> String.concat ""


    // can make this list shorter
    // can perhaps make in an arry
    let rec compact (input: (int option) list) : int list =
        match input with 
            | [] -> []
            | Some h :: t -> h :: compact t 
            | [None] -> []
            | None :: t -> 
                let last = List.last t
                let newList = List.removeAt (t.Length - 1) t
                match last with 
                    | Some x -> x :: compact newList 
                    | None -> compact newList 
    

    [<Fact>]
    let test3 () =
        let pattern = example |> parseInput |> expandPattern 
        Assert.Equivalent("0..111....22222", pattern |> printPattern) 
        let pattern = "input1.txt" |> readInit |> parseInput |> expandPattern 
        Assert.Equivalent("00...111...2...333.44.5555.6666.777.888899", pattern |> printPattern) 

        let compacted = example |> parseInput |> expandPattern |> compact
        Assert.Equal("022111222", compacted |> printCompact)

    [<Fact>]
    let test2 () = 
        let exampleParsed = parseInput example
        Assert.Equivalent([1;2;3;4;5], exampleParsed)

        let input = readInit "input1.txt" 
        Assert.Equal(19, input.Length) 
      
    //files and free space on the disk. The digits alternate between indicating the length of a file and the length of free space.
    // FILE - FREE SPCE
    // is order FILES before rearranging, stari

    // 1 3 5 -> 0 111 222222(5)


module Program = let [<EntryPoint>] main _ = 0