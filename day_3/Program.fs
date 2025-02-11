open System.Text.RegularExpressions

module Input =
    open System.IO
    open Xunit 

    let findMatches input = 
        [for regexMatch in (new Regex(@"mul\((?<num1>\d{1,3}),(?<num2>\d{1,3})\)")).Matches(input) do
            yield (regexMatch.Groups.["num1"].Value |> int, regexMatch.Groups.["num2"].Value |> int) ]
            
    let rec processWithDoAndDont (input: string) =
        let txt = input.Split("don't()", 2)
        let sum a = a |> findMatches |> Seq.sumBy (fun (x,y) -> x * y) 
        match txt with 
            | [|t|]  -> sum t
            | [|t;r|] -> sum t + processWithDoAndDont (r.Split("do()", 2)[1])
            | _ -> failwith("You messed up something")

    [<Fact>]
    let test2 () = 
        let input = File.ReadAllText "input1.txt" 
        let input2 = File.ReadAllText "input2.txt" 
        Assert.Equal(48, input |> processWithDoAndDont)
        Assert.Equal(90044227, input2 |> processWithDoAndDont)

module Program = let [<EntryPoint>] main _ = 0