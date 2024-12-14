type Speed = int*int
type Position = int*int
type Robot = Position * Speed
type BoardSize = int*int
let board1 = (11,7)
let board2 = (101,103) 
let printBoard ((boardLength, boardHeight): BoardSize) (robots: Robot list)= 
    printfn "%A" (robots |> List.length)
    printfn "Length %A Height %A" boardLength boardHeight 
    printfn "%A" robots
    for y in 0..boardHeight-1 do
        for x in 0..boardLength-1 do
            let robotCount = List.filter (fun ((x',y'),_) -> x = x' && y = y') robots |> List.length 
//                printfn "%A" robotCount
            printf "%s" (if robotCount > 0 then (string robotCount) else ".")
        printfn ""


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

module Game = 
    open Xunit

    let simRound (board: BoardSize) ((p,v): Robot ) (t: int): Robot = 
        let (x,y) = p
        let (vx,vy) = v
        let (w,h) = board
        // int64 here later if things go wrong
        let x' = (x+vx*t) % w
        let y' = (y+vy*t) % h
        let x'' = if (x' < 0) then w+x' else x'
        let y'' = if (y' < 0) then h+y' else y'
        (x'',y''), v

    let simRounds (board: BoardSize) (robots: Robot list) (t: int): Robot list = 
        robots |> List.map (fun r -> simRound board r t)
    
    let groupInQuadrant ((boardLength, boardHeight): BoardSize) ((p,_): Robot): int =
        match p with
            | (x,y) when (x < boardLength / 2 && y < boardHeight/ 2) -> 1
            | (x,y) when (x > boardLength / 2 && y < boardHeight/ 2) -> 2
            | (x,y) when (x > boardLength / 2 && y > boardHeight/ 2) -> 3
            | (x,y) when (x < boardLength / 2 && y > boardHeight/ 2) -> 4
            | _ -> 5

    let quadrantProduct (board: BoardSize) (robots: Robot list) : int =
        let robotsInQuadrants = robots |> List.countBy (groupInQuadrant board) |> List.filter (fun (q,_) -> q <> 5)
        // product of elements in list
        robotsInQuadrants |> List.fold (fun acc (_,count) -> acc * count) 1

    let isLineSymmetric (axis: int) (robots: Robot list) : bool =
        true

    let symmetricAboutMiddle (board: BoardSize) (robots: Robot list) : bool =
        // positions are (x,y) where x is x og y is like -y
        // middleaxis is symmatric about x axis at middle
        let middleAxis = board |> fst |> (/) 2
        // lines have the same y
        let lines = robots |> List.groupBy (fun ((_,y),_) -> y)
        lines |> List.forall (fun (_,line) -> isLineSymmetric middleAxis line) 


        // find simpler forms first, they might arrange in simpler, non xmastree like forms
    let xmasTree (board: BoardSize) (robots: Robot list) : bool =
        symmetricAboutMiddle board robots 

    [<Fact>]
    let testRobot () =
        let robot = ((2,4),(2,-3))
        let afterRound0,_ = simRound board1 robot 0
        Assert.Equal((2,4), afterRound0) 
        let afterRound1,_ = simRound board1 robot 1
        Assert.Equal((4,1), afterRound1) 
        let afterRound1,_ = simRound board1 robot 1
 //       printBoard board1 [afterRound1,(0,0)] 
        Assert.Equal((4,1), afterRound1) 
        let afterRound2,_ = simRound board1 robot 2
//        printBoard board1 [afterRound2,(0,0)] 
        let afterRound5,_ = simRound board1 robot 5
        printBoard board1 [afterRound5,(0,0)] 
///        Assert.Equal((4,1), afterRound1) 

module Runner = 
    open Game
    open Input
    open Xunit
    [<Fact>]
    let testRobot () =
        let robots = readInit "input1.txt" 
        let afterRound100 = simRounds board1 robots 100
        printBoard board1 afterRound100 |> ignore
        Assert.Equal(12, quadrantProduct board1 afterRound100)

    [<Fact>]
    let testRobotReal () =
        let robots = readInit "input2.txt" 
        let afterRound100 = simRounds board2 robots 100
        Assert.Equal(231019008 , quadrantProduct board2 afterRound100)

  // idea run backwards from a xmas tree
  // we do not which robots are in which position, so not possible
  // the system is not inversible when we do not know the speeds, which robot is where

module Program = let [<EntryPoint>] main _ = 0