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

    let printPatternA (input: int option []): string = 
        input |> Array.map (fun x -> match x with | Some x -> string x | None -> ".") |> String.concat ""


    let printCompact (input: int list): string = 
        input |> List.map (fun x -> match x with | x -> string x) |> String.concat ""

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

    let findSliceIndex (slice: int option []) (array: int option []) =
        let sliceLength = slice.Length
        array
        |> Seq.windowed sliceLength
        |> Seq.mapi (fun i window -> if window = slice then Some i else None)
        |> Seq.choose id
        |> Seq.tryHead


    let compact2 (input: (int option) list) : (int option) list =
        let inputArray = input |> List.toArray
        let n = input |> List.length

        let mutable i = n - 1
        while i > 0 do
            match inputArray.[i] with
                | None -> i <- i - 1 
                | Some x -> 
                          let xs = inputArray.[i - 10 .. i] |> Array.rev |> Array.takeWhile (fun y -> y = Some x)
                          let requiredDots = xs.Length
                          let dotsArray = Array.replicate requiredDots None
                          match findSliceIndex dotsArray inputArray[0 .. i]  with 
                            | Some index -> inputArray.[index .. index + requiredDots - 1] <- xs 
                                            inputArray.[i - requiredDots + 1 .. i] <- dotsArray
                                            i <- i - xs.Length 
                            | None -> i <- i - xs.Length 
            printPatternA inputArray |> printfn " input %A" 
        inputArray |> Array.toList


    let sumIndexed (input: int list) : int64 =
        input |> List.mapi (fun i x -> int64 (i * x)) |> List.sum
    let sumIndexed2 (input: int option list) : int64 =
        input |> List.mapi (fun i x -> match x with | Some x -> int64 (i * x) | None -> 0L) |> List.sum


    [<Fact>]
    let test3 () =
        let pattern = example |> parseInput |> expandPattern 
        Assert.Equivalent("0..111....22222", pattern |> printPattern) 
        let pattern = "input1.txt" |> readInit |> parseInput |> expandPattern 
        Assert.Equivalent("00...111...2...333.44.5555.6666.777.888899", pattern |> printPattern) 

        let compacted = example |> parseInput |> expandPattern |> compact
        Assert.Equal("022111222", compacted |> printCompact)

        let pattern1= "input1.txt" |> readInit |> parseInput |> expandPattern 
        let compacted = pattern1 |> compact
        Assert.Equal("0099811188827773336446555566", compacted |> printCompact)
        Assert.Equal(1928L, pattern1 |> compact|> sumIndexed)
        Assert.Equal(2858L, pattern1 |> compact2|> sumIndexed2)

//        let compacted = "input2.txt" |> readInit |> parseInput |> expandPattern |> compact
//        Assert.Equal(1928, compacted |> sumIndexed)
        

    [<Fact>]
    let test2 () = 
        let exampleParsed = parseInput example
        Assert.Equivalent([1;2;3;4;5], exampleParsed)

        let input = readInit "input1.txt" 
        Assert.Equal(19, input.Length) 
      
module Program = 
    open Input
    let [<EntryPoint>] main _ = 
        printfn "Hei"
        let compacted = "input2.txt" |> readInit |> parseInput |> expandPattern  |> compact2
        printfn "%A" (compacted |> sumIndexed2)
        System.Console.ReadKey() |> ignore
        0