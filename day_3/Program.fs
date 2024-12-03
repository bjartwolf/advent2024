open System.Text.RegularExpressions

module Input =
    open System
    open System.IO
    open Xunit 

    let findMatches input = 
        [for regexMatch in (new Regex(@"mul\((?<num1>\d{1,3}),(?<num2>\d{1,3})\)")).Matches(input) do
            yield (regexMatch.Groups.["num1"].Value |> Int32.Parse, regexMatch.Groups.["num2"].Value |> Int32.Parse) ]
            
    let rec processWithDoAndDont (input: string) =
        let txt = input.Split("don't()", 2, StringSplitOptions.None)
        let sum = txt.[0] |> findMatches |> Seq.sumBy (fun (x,y) -> x * y) 
        if txt.Length = 1 then
            sum
        else
            let nextLegalPart = txt.[1].Split("do()", 2, StringSplitOptions.None)
            sum + processWithDoAndDont nextLegalPart.[1]

    [<Fact>]
    let test2 () = 
        let input = File.ReadAllText "input1.txt" 
        let input2 = File.ReadAllText "input2.txt" 
        Assert.Equal(48, input |> processWithDoAndDont)
        Assert.Equal(90044227, input2 |> processWithDoAndDont)

module Program = let [<EntryPoint>] main _ = 0