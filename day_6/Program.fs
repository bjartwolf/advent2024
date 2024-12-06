module Input =
    open System
    open System.IO
    open Xunit 

    type Position = int *int
    type Tile = Free | Visited | Obstacle 
    type Direction = W | N | E | S 
    type World = Map<Position, Tile>

    let readInit (filePath: string): string [] = 
        IO.File.ReadAllLines(filePath)

    let parsePipeMap (mapinput: string[]): (World*Position)=
            let obstacles: (Position*Tile) list = [
                for i in 0 .. (mapinput.Length - 1) do
                    let chars = mapinput[i].ToCharArray()
                    for charIndex in 0 .. (chars.Length - 1) do
                        let char = chars[charIndex]
                        match char with 
                              | '#' -> yield ((i,charIndex), Obstacle)
                              | _ -> yield ((i,charIndex),Free)
            ]
            let startPosition = [
                for i in 0 .. (mapinput.Length - 1) do
                    let chars = mapinput[i].ToCharArray()
                    for charIndex in 0 .. (chars.Length - 1) do
                        let char = chars[charIndex]
                        match char with 
                              | '^' -> yield (i,charIndex)
                              | _ -> () 
            ] 
            let pipeMap = Map.ofList obstacles 
            (pipeMap, startPosition.Head)


    let move (direction: Direction) ((y,x): Position): Position  =
        match direction with 
            | N -> (y-1,x)
            | W -> (y,x-1)
            | E -> (y,x+1)
            | S -> (y+1,x)


    [<Fact>]
    let test2 () = 
        let world, position = "input1.txt" |> readInit |> parsePipeMap
        Assert.Equal((6,4), position) 
        Assert.Equal(Obstacle, Map.find (0,4) world) 
        Assert.Equal(Free, Map.find (0,3) world) 

module Program = let [<EntryPoint>] main _ = 0