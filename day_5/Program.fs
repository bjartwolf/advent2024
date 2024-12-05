module Input =
    open System
    open System.IO
    open Xunit 

    let parseRule (rule: string): int*int =
        let splitRule = rule.Split("|", StringSplitOptions.RemoveEmptyEntries)
        (int splitRule.[0], int splitRule.[1])

    let readInit (filePath: string): (int*int) list * (int list) list = 
        let txt = File.ReadAllText filePath 
        let splitTxt = txt.Split(System.Environment.NewLine + System.Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
        let rules = splitTxt.[0].Split(System.Environment.NewLine, StringSplitOptions.RemoveEmptyEntries) |> Array.map parseRule |> Array.toList
        let lists = splitTxt.[1].Split(System.Environment.NewLine, StringSplitOptions.RemoveEmptyEntries) 
                                        |> Array.map (fun x -> x.Split(",", StringSplitOptions.RemoveEmptyEntries))
                                        |> Array.map (fun x -> x |> Array.map int |> Array.toList) |> Array.toList 
        (rules , lists) 

    [<Fact>]
    let test2 () = 
        let (rules, lists ) = readInit "input1.txt" 
        Assert.Equal(21, rules.Length) 

module Program = let [<EntryPoint>] main _ = 0