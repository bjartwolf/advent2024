module Input =
    open System
    open System.IO
    open Xunit 

    let example = "12345"
    let readInit (filePath: string): string = File.ReadAllText(filePath) 

    let parseInput (input: string): int list =
        input |> Seq.toList|> Seq.map (fun c -> int c - int '0' ) |> Seq.toList

    let expandPattern (input: int list) : (int option) list =
        let mutable result = []
        let mutable idNr = 0
        let mutable i = 0

        while i < List.length input do
            if i % 2 = 0 then
                result <- result @ List.replicate (List.item i input) (Some idNr)
                idNr <- idNr + 1
            else
                result <- result @ List.replicate (List.item i input) None
            i <- i + 1

        result

    let printPattern (input: int option list): string = 
        input |> List.map (fun x -> match x with | Some x -> string x | None -> ".") |> String.concat ""

    let printCompact (input: int list): string = 
        input |> List.map (fun x -> match x with | x -> string x) |> String.concat ""

    let rec fixTails (input: (int option) list): (int option) list =
        if (List.isEmpty input) then
           [] 
        else 
            let last = List.last input
            if (last = None) then
                let newList = List.removeAt (input.Length - 1) input
                fixTails newList
            else 
                input


    // can make this list shorter
    // can perhaps make in an arry
    let compact (input: (int option) list) : int list =
        let inputArray = input |> List.toArray
        let numberOfNums = input |> List.choose id |> List.length
        let result: int[] = Array.zeroCreate inputArray.Length 
        for i in 0 .. (input.Length - 1) do
            match inputArray.[i] with
                | Some x -> result.[i] <- x 
                | None -> let indexOfNum = Array.tryFindIndexBack (fun x -> Option.isSome x ) inputArray
                          if Option.isSome indexOfNum then
                              result.[i] <- inputArray.[indexOfNum.Value].Value
                              inputArray.[indexOfNum.Value] <- None
        result[0 .. numberOfNums - 1] |> Array.toList
        (*
        match input with 
            | [] -> []
            | [Some h] -> [h] 
            | Some h :: t -> h :: compact t 
            | [None] -> []
            | None :: t -> 
//                printPattern t |> printfn "%A" 
                let list = fixTails t 
                if List.isEmpty list then [] 
                else
                    let newList = List.removeAt (list.Length - 1) t
                    let last = List.last list
                    match last with 
                        | Some x -> 
                            x :: compact newList 
                        | None -> 
                            compact newList 
    *)
    let sumIndexed (input: int list) : int64 =
        input |> List.mapi (fun i x -> int64 (i * x)) |> List.sum

    [<Fact>]
    let test3 () =
        let pattern = example |> parseInput |> expandPattern 
        Assert.Equivalent("0..111....22222", pattern |> printPattern) 
        let pattern = "input1.txt" |> readInit |> parseInput |> expandPattern 
        Assert.Equivalent("00...111...2...333.44.5555.6666.777.888899", pattern |> printPattern) 

        let compacted = example |> parseInput |> expandPattern |> compact
        Assert.Equal("022111222", compacted |> printCompact)

        let compacted = "input1.txt" |> readInit |> parseInput |> expandPattern |> compact
        Assert.Equal("0099811188827773336446555566", compacted |> printCompact)
        Assert.Equal(1928L, compacted |> sumIndexed)

//        let compacted = "input2.txt" |> readInit |> parseInput |> expandPattern |> compact
//        Assert.Equal(1928, compacted |> sumIndexed)
        

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


module Program = 
    open Input
    let [<EntryPoint>] main _ = 
        printfn "Hei"
        let compacted = "input2.txt" |> readInit |> parseInput |> expandPattern  |> compact
        printfn "%A" (compacted |> sumIndexed)
        System.Console.ReadKey() |> ignore
        0