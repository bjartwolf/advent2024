module Input =
    open System
    open System.IO
    open Xunit 

    type Position = int * int
    type Kart = Map<Position, int>

    let readInit (filePath: string): string[] = File.ReadAllLines(filePath)
    let parseInit (input: string[]) : Kart = 
        let foo: (Position*int) list = [
            for i in 0 .. (input.Length - 1) do
                let chars = input[i].ToCharArray()
                for charIndex in 0 .. (chars.Length - 1) do
                    let char = chars[charIndex]
                    yield ((i,charIndex), Int32.Parse(char.ToString()))
        ] 
        foo |> Map.ofList


    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(8, input.Length) 
        let mapInput = [|"123"; "456"; "789"|]
        let map = parseInit mapInput
        Assert.Equal(5, Map.find (1,1) map)
        Assert.Equal(9, Map.find (2,2) map)

module Program = let [<EntryPoint>] main _ = 0