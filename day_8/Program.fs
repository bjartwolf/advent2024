module Input =
    open System
    open System.IO
    open Xunit 

    type Position = int * int
    type Tile = Empty | Antenna of char 
    type World = Map<Position, Tile>

    let readInit (filePath: string): string [] = 
        IO.File.ReadAllLines(filePath)

    let parsePipeMap (mapinput: string[]): World =
        [
            for i in 0 .. (mapinput.Length - 1) do
                let chars = mapinput[i].ToCharArray()
                for charIndex in 0 .. (chars.Length - 1) do
                    let char = chars[charIndex]
                    match char with 
                          | '.' -> yield  ((i,charIndex),Empty)
                          | antenna -> yield  ((i,charIndex), Antenna antenna)
        ] |> Map.ofList

    let printWorld (n: int) (world: World) = 
        for i in 0 .. n - 1 do
            for j in 0 .. n - 1 do
                match world.TryFind((i,j)) with
                | Some Empty -> printf "."
                | Some (Antenna a) -> printf "%c" a
                | None -> failwith "oops" 
            printfn ""

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(12, input.Length) 
        let map = parsePipeMap input
        printWorld input.Length map
        Assert.Equal(12*12, map.Count) 

module Program = let [<EntryPoint>] main _ = 0