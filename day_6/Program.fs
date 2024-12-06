open System

module Input =
    open System
    open System.IO
    open Xunit 

    type Position = int *int
    type Direction = W | N | E | S 
    type Tile = Free | Visited of Direction list | Obstacle 
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

    let turn (direction:Direction): Direction = 
        match direction with 
            | N -> E
            | W -> N
            | S -> W
            | E -> S

    let move (direction: Direction) ((y,x): Position): Position  =
        match direction with 
            | N -> (y-1,x)
            | W -> (y,x-1)
            | E -> (y,x+1)
            | S -> (y+1,x)

    let walkMap (world: World) (position: Position): World = 
        let rec walk (world: World) (position: Position) (direction: Direction): World = 
            let pos_ahead = move direction position
            let tile_ahead = Map.tryFind pos_ahead world 
            match tile_ahead with 
                | Some(Free)  -> 
                    let newWorld = Map.add pos_ahead (Visited [direction]) world
                    walk newWorld pos_ahead direction
                | Some(Visited visitedDirections ) -> 
                    let visitedDirections' = direction::visitedDirections
                    let newWorld = Map.add pos_ahead (Visited visitedDirections') world
                    walk newWorld pos_ahead direction
                | Some(Obstacle) -> 
                    walk world position (turn direction)
                | None -> world
        let world' = Map.add position (Visited [N]) world
        walk world' position N 

        // including cycledetection, return true if cycle detected, false if not
    let walkMapC (world: World) (position: Position): (World * bool) = 
        let rec walkC (world: World) (position: Position) (direction: Direction): (World*bool) = 
            let pos_ahead = move direction position
            let tile_ahead = Map.tryFind pos_ahead world 
            match tile_ahead with 
                | Some(Visited visitedDirections ) when List.contains direction visitedDirections -> (world, true)
                | Some(Free)  -> 
                    let newWorld = Map.add pos_ahead (Visited [direction]) world
                    walkC newWorld pos_ahead direction
                | Some(Visited visitedDirections ) -> 
                    let visitedDirections' = direction::visitedDirections
                    let newWorld = Map.add pos_ahead (Visited visitedDirections') world
                    walkC newWorld pos_ahead direction
                | Some(Obstacle) -> 
                    walkC world position (turn direction)
                | None -> (world, false)
        let world' = Map.add position (Visited [N]) world
        walkC world' position N 

    // 10 eller 130 stort
    let generateMaps (world: World) (size: int) (startPosition: Position ): World seq = 
        [for i in 0 .. (size - 1) do
             for j in 0 .. (size - 1) do
                let tile = Map.tryFind (i,j) world 
                match tile with 
                    | Some(Free) when (i,j) <> startPosition -> 
                        let map' = Map.add (i,j) Obstacle world
                        yield map'
                    | _ -> ()
        ] 

    let findNrOfCycleMaps (world: World) (size: int) (startPosition: Position ): int = 
        let possibleMaps = generateMaps world size startPosition
        possibleMaps |> Seq.map (fun m -> walkMapC m startPosition)
                     |> Seq.filter (fun (_,cycle) -> cycle) 
                     |> Seq.length
         

    // assumption always start looking north
    [<Fact>]
    let test2 () = 
        let world, position = "input1.txt" |> readInit |> parsePipeMap
        Assert.Equal((6,4), position) 
        Assert.Equal(Obstacle, Map.find (0,4) world) 
        Assert.Equal(Free, Map.find (0,3) world) 
        Assert.Equal(100-9, generateMaps world 10 position |> Seq.length)  // CAN WE PUT AN OBSTACLE ON START? Nah...
        Assert.Equal(6, findNrOfCycleMaps world 10 position)
        let walkedWorld = walkMap world position 
        Assert.Equal(41, walkedWorld |> Map.filter (fun _ v -> match v with | Visited _ -> true | _ -> false) |> Map.count)
        let world2, position2 = "input2.txt" |> readInit |> parsePipeMap
        let walkedWorld2 = walkMap world2 position2
        Assert.Equal(4433, walkedWorld2 |> Map.filter (fun _ v ->  match v with | Visited _ -> true | _ -> false) |> Map.count)
//        Assert.Equal(6, findNrOfCycleMaps world2 130 position2)

module Program = 
    open Input
    let [<EntryPoint>] main _ = 
        let world2, position2 = "input2.txt" |> readInit |> parsePipeMap
        let walkedWorld2 = walkMap world2 position2
        let part1 = walkedWorld2 |> Map.filter (fun _ v ->  match v with | Visited _ -> true | _ -> false) |> Map.count
        printfn "Part1: %A" part1
        let part2 = findNrOfCycleMaps world2 130 position2
        printfn "Part2: %A" part2
        Console.ReadLine() |> ignore
        0