module Input =
    open System
    open System.IO
    open Xunit 

    type Position = int * int
    type Kart = Map<Position, int>
    type Trailheads = Position list

    let readInit (filePath: string): string[] = File.ReadAllLines(filePath)
    let parseInit (input: string[]) : Kart*Trailheads = 
        let foo: (Position*int) list = [
            for i in 0 .. (input.Length - 1) do
                let chars = input[i].ToCharArray()
                for charIndex in 0 .. (chars.Length - 1) do
                    let char = chars[charIndex]
                    yield ((i,charIndex), Int32.Parse(char.ToString()))
        ] 
        let trailHeads = [
            for i in 0 .. (input.Length - 1) do
                let chars = input[i].ToCharArray()
                for charIndex in 0 .. (chars.Length - 1) do
                    let char = chars[charIndex]
                    if char = '0' then yield (i, charIndex)
        ]
        foo |> Map.ofList, trailHeads

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(8, input.Length) 
        let mapInput = [|"003"; "456"; "709"|]
        let map,trailHeads  = parseInit mapInput
        Assert.Equal(5, Map.find (1,1) map)
        Assert.Equal(9, Map.find (2,2) map)
        Assert.Equivalent([(0,0);(0,1);(2,1)], trailHeads)

module Program = let [<EntryPoint>] main _ = 0