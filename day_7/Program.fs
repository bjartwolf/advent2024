module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): (int*(int list)) list = 
        let parseLine (line: string): int list = 
            line.Split(" ") 
                 |> Array.where(fun s -> String.IsNullOrEmpty(s) |> not)
                 |> Array.map (fun n -> Int32.Parse(n))
                 |> Array.toList

        File.ReadAllLines(filePath) |> Array.toList
            |> List.map (fun line -> line.Split(":"))
            |> List.map (fun parts -> parts.[0] |> int, parseLine parts.[1]) 

    type Line = int * int list
    let possibleValues ((num, nums): Line) : int seq =
        let rec possibleValuesInner (res: int) ((num, nums): Line) : int seq =
            [ 
                match nums with
                    | [] -> yield res
                    | h::t -> yield! possibleValuesInner (res + h ) (num, t)
                              yield! possibleValuesInner (res * h ) (num, t)
            ]
        possibleValuesInner (List.head nums) (num, List.tail nums)

    [<Fact>]
    let possibles () = 
        Assert.Contains(190, possibleValues (190, [10; 19]) |> Seq.toList)
        Assert.Contains(29, possibleValues (190, [10; 19]) |> Seq.toList)
        Assert.Contains(3267, possibleValues (3267, [81; 40;27]) |> Seq.toList)
 
    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(9, input.Length) 

module Program = let [<EntryPoint>] main _ = 0