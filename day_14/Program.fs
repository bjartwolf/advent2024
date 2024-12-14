type Speed = int*int
type Position = int*int
type Robot = Position * Speed
type BoardSize = int*int
let board1 = (11,7)
let board2 = (101,103) 

module Input =
    open System
    open System.IO
    open Xunit 

    //p=0,4 v=3,-3
    let parse (line: string): Position*Speed =
        let parts = line.Split(" ")
        let part1 = parts.[0].Split("=").[1].Split(",")
        let part2 = parts.[1].Split("=").[1].Split(",")
        let robot1 = (int part1.[0], int part1.[1])
        let robot2 = (int part2.[0], int part2.[1])
        (robot1, robot2)

    let readInit (filePath: string): Robot list = 
        let lines = File.ReadAllLines(filePath)
        let robots = lines |> Array.map parse
                           |> Array.toList
        robots

    [<Fact>]
    let testRobots() = 
        let robots = readInit "input1.txt" 
        printfn "%A" robots 
        Assert.Equal(12, robots.Length)

    [<Fact>]
    let test2 () = 
        let robots = readInit "input1.txt" 
        printfn "%A" robots 
        Assert.Equal(12, robots.Length) 

    let printBoard ((boardLength, boardHeight): BoardSize) (robots: Robot list)= 
        printfn "%A" (robots |> List.length)
        printfn "Length %A Height %A" boardLength boardHeight 
        printfn "%A" robots
        for y in 0..boardHeight do
            for x in 0..boardLength do
                let robotCount = List.filter (fun ((x',y'),_) -> x = x' && y = y') robots |> List.length 
//                printfn "%A" robotCount
                printf "%s" (if robotCount > 0 then (string robotCount) else ".")
            printfn ""

module Game = 
    open Xunit

    let simRound (board: BoardSize) ((p,v): Robot ) (t: int): Robot = 
        let (x,y) = p
        let (vx,vy) = v
        let (w,h) = board
        // int64 here later
        let x' = (x+vx*t) % w
        let y' = (y+vy*t) % h
        ((x',y'),v)

    let simRounds (board: BoardSize) (robots: Robot list) (t: int): Robot list = 
        robots |> List.map (fun r -> simRound board r t)
    
    let groupInQuadrant ((boardLength, boardHeight): BoardSize) ((p,_): Robot): int =
        match p with
            | (x,y) when (x < boardLength / 2 && y < boardHeight/ 2) -> 1
            | (x,y) when (x > boardLength / 2 && y < boardHeight/ 2) -> 2
            | (x,y) when (x > boardLength / 2 && y > boardHeight/ 2) -> 3
            | (x,y) when (x < boardLength / 2 && y > boardHeight/ 2) -> 4

    let quadrantProduct (board: BoardSize) (robots: Robot list) : int =
        let robotsInQuadrants = robots |> List.countBy (groupInQuadrant board)
        // product of elements in list
        robotsInQuadrants |> List.fold (fun acc (_,count) -> acc * count) 1

    [<Fact>]
    let testRobot () =
        let robot = ((2,4),(2,-3))
        let afterRound0,_ = simRound board1 robot 0
        Assert.Equal((2,4), afterRound0) 
        let afterRound1,_ = simRound board1 robot 1
        Assert.Equal((4,1), afterRound1) 

module Runner = 
    open Game
    open Input
    open Xunit
    [<Fact>]
    let testRobot () =
        let robots = readInit "input1.txt" 
        let afterRound100 = simRounds board1 robots 100
        printBoard board1 afterRound100 |> ignore
//        Assert.Equal(12, quadrantProduct board1 afterRound100)

    ()

module Program = let [<EntryPoint>] main _ = 0