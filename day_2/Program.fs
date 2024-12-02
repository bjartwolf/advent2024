module Input =
    open System
    open System.IO

    let readInit (filePath: string): int list list = 
        let reports = File.ReadAllLines(filePath) 
        let splitReports = reports |> Array.map (fun l -> l.Split(" "))
        splitReports |> Array.map(fun r -> r |> Array.where (fun x -> x.Length > 0) |> Array.map (fun l -> Int32.Parse(l)) |> Array.toList) |> Array.toList 

module Day2 = 
    let testLine0 = [7;6;4;2;1]
    let testLine1 = [1;2;7;8;9]
    let testLine2 = [9;7;6;2;1]
    let testLine3 = [1;3;2;4;5]
    let testLine4 = [8;6;4;4;1]
    let testLine5 = [1;3;6;7;9] 
    
    let rec derivative (report: int list): int list =
        match report with
        | [] -> []
        | [h1] -> []
        | h1::h2::t -> h2-h1 :: derivative (h2::t)

    let safe (report: int list) : bool = 
        let derivatives = derivative report
        let increasing = List.head derivatives > 0
        if increasing then
            derivatives |> List.forall (fun x -> x > 0 && x <= 3)
        else 
            derivatives |> List.forall (fun x -> x < 0 && x >= -3)

    let countSafe (reports: int list list): int = 
        reports |> List.filter (fun r -> safe r) |> List.length


module Test = 
    open Day2
    open Input
    open Xunit 

    // report = line
    // line består av levels
    // which lines/ reports are safe

    // gradually increasing / or decreasing
    // at least one, at most three
    // monotontically increasing or decreasing

    [<Fact>]
    let dLine1 () = 
        let der0 = derivative testLine0 
        Assert.Equivalent([-1;-2;-2;-1], der0)
        let der1 = derivative testLine1
        Assert.Equivalent([1;5;1;1], der1)
        Assert.Equivalent([-2;-1;-4;-1], derivative testLine2)
 
    [<Fact>]
    let safe () = 
        Assert.True(safe testLine0)
        Assert.False(safe testLine1)
        Assert.False(safe testLine2)
        Assert.False(safe testLine3)
        Assert.False(safe testLine4)
        Assert.True(safe testLine5)

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equivalent([7;6;4;2;1], input.[0]) 
        Assert.Equivalent([1;3;6;7;9], input.[5]) 
        Assert.Equal(2, countSafe input)
        let input2 = readInit "input2.txt" 
        Assert.Equal(2, countSafe input2)

module Program = 
    let [<EntryPoint>] main _ =
        0