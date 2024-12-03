open System.Text.RegularExpressions

module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): string = 
        let txt = File.ReadAllText(filePath) 

        txt

    // regex - mul(X,Y), where X and Y are each 1-3 digit numbers
    let findMatches(input: string) : (int*int) seq =
        let pattern = @"mul\((?<num1>\d{1,3}),(?<num2>\d{1,3})\)"
        let regex = new Regex(pattern)
        let regexMatches = regex.Matches(input)

        [for regexMatch in regexMatches do
            let number1 = regexMatch.Groups.["num1"].Value |> Int32.Parse
            let number2 = regexMatch.Groups.["num2"].Value |> Int32.Parse
            yield (number1, number2)]

    let product (input: (int*int) seq): int64 =
        input |> Seq.map (fun (x,y) -> int64 (x * y)) |> Seq.sum

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(71, input.Length) 
        Assert.Equal(4, input |> findMatches |> Seq.length) 
        Assert.Equal((2,4), input |> findMatches |> Seq.toList |> Seq.head) 
        Assert.Equal(161L, input |> findMatches |> product)
        let input2 = readInit "input2.txt" 
        Assert.Equal(19723, input2.Length) 
        Assert.False(input.Contains(System.Environment.NewLine)) 
        Assert.Equal(184511516L, input2 |> findMatches |> product)


module Program = let [<EntryPoint>] main _ = 0