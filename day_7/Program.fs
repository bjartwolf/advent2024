module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): (int64*(int64 list)) list = 
        let parseLine (line: string): int64 list = 
            line.Split(" ") 
                 |> Array.where(fun s -> String.IsNullOrEmpty(s) |> not)
                 |> Array.map (fun n -> Int64.Parse(n))
                 |> Array.toList

        File.ReadAllLines(filePath) |> Array.toList
            |> List.map (fun line -> line.Split(":"))
            |> List.map (fun parts -> parts.[0] |> int64, parseLine parts.[1]) 

    type Line = int64 * int64 list
    let possibleValues ((num, nums): Line) : int64 seq =
        let rec possibleValuesInner (res: int64) ((num, nums): Line) : int64 seq =
            [ 
                match nums with
                    | [] -> yield res
                    | h::t -> yield! possibleValuesInner (res + h ) (num, t)
                              yield! possibleValuesInner (res * h ) (num, t)
            ]
        possibleValuesInner (List.head nums) (num, List.tail nums)

    let containsSolution (line: Line ) : bool = 
        let possibilities = possibleValues line 
        let num, _ = line
        possibilities |> Seq.exists (fun res -> res = num)

    let sumOfSolutions (lines: Line list) : int64 = 
        lines |> List.filter containsSolution |> List.map (fun (num, _) -> num) |> List.sum

        (*
    [<Fact>]
    let solutions () = 
        Assert.True(containsSolution (190, [10; 19]))
        Assert.True(containsSolution (3267, [81;40;27]))


    [<Fact>]
    let possibles () = 
        Assert.Contains(190, possibleValues (190, [10; 19]) |> Seq.toList)
        Assert.Contains(29, possibleValues (190, [10; 19]) |> Seq.toList)
        Assert.Contains(3267, possibleValues (3267, [81; 40;27]) |> Seq.toList)
 *)
    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(9, input.Length) 
        Assert.Equal(3749L, sumOfSolutions input) 
        let input2 = readInit "input2.txt" 
        Assert.Equal(3749L, sumOfSolutions input2) 

module Program = let [<EntryPoint>] main _ = 0