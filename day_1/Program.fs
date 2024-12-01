module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): (int*int) list = 
        use sr = new StreamReader (filePath) 
        let lines = sr.ReadToEnd()
        let numbers = lines.Split(System.Environment.NewLine);
        let splitLines = numbers |> Array.map(fun line -> line.Split("  ")) 
        let nums = splitLines |> Array.map(fun strings  -> Int32.Parse(strings.[0]), Int32.Parse(strings.[1]))
        nums |> Array.toList

    let testData: (int*int) list = [ (3,4); (4,3); (2,5); (1,3); (3,9); (3,3)]

    let splitAndSortList (pairList: (int*int) list): (int list)*(int list) = 
        let firstList = pairList |> List.map fst |> List.sort
        let secondList = pairList |> List.map snd |> List.sort
        (firstList, secondList)

    let findDistance (l1: int list,l2: int list) : int =
        let lst = List.zip l1 l2
        lst |> List.map (fun (n1,n2) -> abs(n1 - n2))  
            |> List.sum

    let countOccurrances (l1: int list, l2: int list): ((int * int) list ) = 
        l1 |> List.map (fun num -> (num, l2 |> List.filter (fun n2 -> n2 = num) |>  List.length ))

    let productOfOccurances (counts: (int*int) list) : int =
        counts |> List.map (fun (num,count) -> num*count) |> List.sum


    [<Fact>]
    let test () = 
        let input = readInit "input1.txt" 
        Assert.Equal(6, input.Length) 
        Assert.Equivalent(testData, input)
        let (l1,l2) = splitAndSortList testData
        Assert.Equivalent(1, l1 |> List.head)
        Assert.Equal(11, findDistance (l1,l2))
        Assert.Equal(31, countOccurrances (l1,l2) |> productOfOccurances)

    [<Fact>]
    let test2 () = 
        let input = readInit "input2.txt" 
        let (l1,l2) = splitAndSortList input 
        Assert.Equal(2378066, findDistance (l1,l2))
        Assert.Equal(18934359, countOccurrances (l1,l2) |> productOfOccurances) 

module Program = let [<EntryPoint>] main _ = 0
