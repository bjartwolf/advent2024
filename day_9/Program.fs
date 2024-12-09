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

    let compact2 (input: (int option) list) : (int option) list =
        let inputArray = input |> List.toArray
        let n = input |> List.length
        let result: int option [] = Array.zeroCreate inputArray.Length 

        let mutable i = 0
        while i < (input.Length - 1) do
            match inputArray.[i] with
                | Some x -> result.[i] <- Some x 
                            i <- i + 1
                | None -> // find how many dotes there are, max 10
                          let numberOfDots = inputArray[i .. i + 10] |> Array.takeWhile (fun x -> Option.isNone x) |> Array.length
                          // find something that fits the dots
                          // need to find consequtive numbers from the back that can fit the dots
                          let mutable foundSlice:int option []= [||]
                          let mutable found = false
                          let mutable j = n - 1
                          if numberOfDots > 0 then
                              while j > i && not found && j > 0 do 
                                  // check to find all equal numbers in a row that fits and skip
                                  // the size of that numbers if there are numbers that does not fit 
                                  let value = inputArray.[j]
                                  match value with 
                                    | None -> j <- j - 1
                                    | Some x -> 
                                      // find all xes in a row at the end of the array
                                      let xs = inputArray.[j - 10 .. j] |> Array.filter (fun y -> y = Some x)
                                      if xs.Length <= numberOfDots then
                                          //let slice = inputArray[n - numberOfDots - j .. n - j] 
                                          if (xs.Length <= numberOfDots) then
                                                  foundSlice <- xs 
                                                  found <- true
                                      j <- j - xs.Length 
                          try
                             if (i + numberOfDots + 1 < inputArray.Length ) && found then 
                                result[i  .. i + foundSlice.Length - 1] <- foundSlice 
                                inputArray[j + 1  .. j + foundSlice.Length] <- Array.create foundSlice.Length None
                          with 
                             | err -> 
//                                printfn "%A %A %A" i numberOfDots foundSlice 
                                printfn "Error: %A" err.Message
                          if numberOfDots > 0 then
                              i <- i + numberOfDots
                          else 
                              i <- i + 1
            printPatternA result |> printfn " RES %A" 
            printPatternA inputArray |> printfn " input %A" 
        result |> Array.toList


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