module Input
open System

let readInit (filePath: string): (int*int) list = 
    System.IO.File.ReadAllLines(filePath) 
        |> Array.map(fun line -> line.Split("  ")) 
        |> Array.map(fun strings -> Int32.Parse(strings.[0]), Int32.Parse(strings.[1]))
        |> Array.toList

module Program = 
    open Day1Core
    let [<EntryPoint>] main _ = 
        let input = readInit "input2.txt" 
        let (l1,l2) = splitAndSortList input 
        printfn "%A" (countOccurrances (l1,l2) |> productOfOccurances)
        0



