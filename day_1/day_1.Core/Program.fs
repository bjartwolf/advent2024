module Day1Core 
open System

let splitOnNewlines (input: string) : string[] =
    input.Split([| '\r'; '\n' |], StringSplitOptions.RemoveEmptyEntries)

let parseInput (input: string): (int*int) list = 
    input
        |> splitOnNewlines
        |> Array.map (fun line -> line.Split("  ")) 
        |> Array.filter (fun line -> line.Length > 0)
        |> Array.map(fun strings -> Int32.Parse(strings.[0]), Int32.Parse(strings.[1]))
        |> Array.toList

let split (pairList: (int*int) list): (int list)*(int list) = 
    pairList |> List.map fst, pairList |> List.map snd 

let splitAndSort (pairList: (int*int) list): (int list)*(int list) = 
    pairList |> List.map fst |> List.sort, pairList |> List.map snd  |> List.sort

let findDistance (l1: int list,l2: int list) : int =
    List.zip l1 l2
        |> List.sumBy (fun (n1,n2) -> abs(n1 - n2))  

let countOccurrances (l1: int list, l2: int list) = 
    let counts = l2 |> List.countBy id |> Map.ofList
    l1 |> List.map (fun num1 -> (num1, Map.tryFind num1 counts |> Option.defaultValue 0)) 

let productOfOccurances (counts: (int*int) list) =  
    counts |> List.fold(fun acc (num,count) -> acc + num*count) 0

let distance (input: string) = 
    input
        |> parseInput
        |> splitAndSort
        |> findDistance 

let productOfInput (input: string) = 
    input
        |> parseInput
        |> split 
        |> countOccurrances
        |> productOfOccurances
