module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): (int*int) list = 
        System.IO.File.ReadAllLines(filePath) 
            |> Array.map(fun line -> line.Split("  ")) 
            |> Array.map(fun strings -> Int32.Parse(strings.[0]), Int32.Parse(strings.[1]))
            |> Array.toList

    let splitAndSortList (pairList: (int*int) list): (int list)*(int list) = 
        (pairList |> List.map fst |> List.sort, pairList |> List.map snd |> List.sort)

    let findDistance (l1: int list,l2: int list) : int =
        List.zip l1 l2
            |> List.sumBy (fun (n1,n2) -> abs(n1 - n2))  

    let countOccurrances (l1: int list, l2: int list) = 
        let counts = l2 |> List.countBy id |> Map.ofList
        l1 |> List.map (fun num1 -> (num1, Map.tryFind num1 counts |> Option.defaultValue 0)) 

    let productOfOccurances (counts: (int*int) list) =  
        counts |> List.fold(fun acc (num,count) -> acc + num*count) 0

    [<Fact>]
    let test () = 
        let input = readInit "input1.txt" 
        Assert.Equal(6, input.Length) 
        let (l1,l2) = splitAndSortList input 
        Assert.Equivalent(1, l1 |> List.head)
        Assert.Equal(11, findDistance (l1,l2))
        Assert.Equal(31, countOccurrances (l1,l2) |> productOfOccurances)

    [<Fact>]
    let test2 () = 
        let input = readInit "input2.txt" 
        let (l1,l2) = splitAndSortList input 
        Assert.Equal(18934359, countOccurrances (l1,l2) |> productOfOccurances) 
        Assert.Equal(2378066, findDistance (l1,l2))

module Program = let [<EntryPoint>] main _ = 0
