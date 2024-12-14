type Speed = int*int
type Position = int*int
type Robot = Position * Speed
type BoardSize = int*int
let board1 = (11,7)
let board2 = (101,103) 
let printBoard ((boardLength, boardHeight): BoardSize) (robots: Robot list)= 
//    printfn "%A" (robots |> List.length)
//    printfn "Length %A Height %A" boardLength boardHeight 
 //   printfn "%A" robots
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
        let x,y = p
        let (vx,vy) = v
        let (x: int64,y:int64) = (int64 x, int64 y)
        let (vx: int64,vy:int64) = (int64 vx, int64 vy)
        let t = int64 t
        let (w,h) = board
        let h = int64 h
        let w = int64 w
        // int64 here later if things go wrong
        let x' = (x+vx*t) % w
        let y' = (y+vy*t) % h
        let x'' = if (x' < 0) then w+x' else x'
        let y'' = if (y' < 0) then h+y' else y'
        (int x'',int y''), v

    let simRounds (board: BoardSize) (robots: Robot list) (t: int): Robot list = 
        robots |> List.map (fun r -> simRound board r t)

    
    let groupInQuadrant ((boardLength, boardHeight): BoardSize) (p: Position): int =
        match p with
            | (x,y) when (x < boardLength / 2 && y < boardHeight/ 2) -> 1
            | (x,y) when (x > boardLength / 2 && y < boardHeight/ 2) -> 2
            | (x,y) when (x > boardLength / 2 && y > boardHeight/ 2) -> 3
            | (x,y) when (x < boardLength / 2 && y > boardHeight/ 2) -> 4
            | _ -> 5

    let isInXmasFormation ((boardLength, boardHeight): BoardSize) (p: Position) : bool = 
        let quadrant = groupInQuadrant (boardLength, boardHeight) p
        match quadrant with 
            | 1 -> true
            | 2 -> true
            | 3 -> true
            | 4 -> true
            | 5 -> true
            | _ -> failwith "should not hape"


    let quadrantProduct (board: BoardSize) (robots: Robot list) : int =
        let robotPositions = robots |> List.map (fun (p,_) -> p)
        let robotsInQuadrants = robotPositions |> List.countBy (groupInQuadrant board) |> List.filter (fun (q,_) -> q <> 5)
        // product of elements in list
        robotsInQuadrants |> List.fold (fun acc (_,count) -> acc * count) 1

    let isLineSymmetric (axis: int) (robots: Position list) : bool =
        // line is symmetric if there is a robot at the other side for each 
        // robot at one side
        // y is always the same, but we could code that without groups
        let side1 = robots|> List.filter (fun (x,_)-> x < axis)
        let side2 = robots|> List.filter (fun (x,_)-> x > axis) |> List.map (fun (x,y) -> (x-axis-1,y))
 //       printfn "axis %A" axis 
//        printfn "robots %A" robots 
//        printfn "side 1 %A" (side1 |> List.distinct)
//        printfn "side 2 %A" (side2 |> List.distinct)
        (side1 |> List.distinct |> List.sort) = (side2 |> List.distinct |> List.sort)

    let symmetricAboutMiddle ((boardWidth, _): BoardSize) (robots: Robot list) : bool =
        if (boardWidth % 2 <> 1) then failwith "Board width must be uneven"
        let middleAxis = boardWidth / 2
  //      printfn "middle %A" middleAxis
        let lines = robots |> List.map (fun (p,v) -> p) |> List.groupBy (fun (_,y) -> y)
        lines |> List.forall (fun (_,robots) -> isLineSymmetric middleAxis robots)

    let tostrings (board: BoardSize)(robots: Position list): string list=
        let (w,h) = board
        [
            for i in 0..h-1 do
                let mutable chars : char []= Array.create w '.'
                for j in 0..w-1 do
                    if List.exists (fun (x,y) -> x = j && y = i) robots then
                        chars.[j] <- 'X'
                yield new System.String(chars)
        ]

    let hasStraightLine (board: BoardSize) (robots: Position list): bool =
        let w,h = board
        let strings = tostrings board robots
        let straight = "XXXXXXXXX"
        List.exists (fun (s:string) -> s.Contains(straight)) strings
         

    let xmasTree (board: BoardSize) (robots: Robot list) : bool =
        let positions = robots |> List.map (fun (p,_) -> p) |> List.distinct
        hasStraightLine board positions 
//        symmetricAboutMiddle board robots 

    let rec simUntilXmasTree (board: BoardSize) (robots: Robot list) (i: int) (maxT: int) : (Robot list*int) =
        printf "%A " i
        if i > maxT then failwith (sprintf "Terminating %A " i)
       // printfn "*** Round %A ***" i
       // printBoard board robots 
        
        let robots' = simRounds board robots i 
        if xmasTree board robots' then (robots', i) 
        else
            simUntilXmasTree board robots (i+1) maxT

    [<Fact>]
    let testRobotSymmetri () =
        let robots: Robot list = [(0,1),(0,1);(2,1),(0,0);(1,1),(0,0)] 
        let robots2: Robot list = [(0,0),(0,0);(1,0),(0,1)] 
        let robots3: Robot list = [(0,0),(0,0);(1,0),(0,1);(3,0),(2,0);(4,0),(2,0);(2,1),(1,0)] 
        let robots4: Robot list = [(0,0),(0,0);(1,0),(0,1);(3,0),(2,0);(4,0),(2,0);(4,1),(1,0)] 
        let robots5: Robot list = [(0,1),(0,0);(1,1),(0,1);(3,1),(2,0);(4,1),(2,0);(2,3),(1,0)] 
        let robots6: Robot list = robots4 @ robots5
        printfn "symmatric"
        printBoard (5,4) robots6
        Assert.True(symmetricAboutMiddle (5,4) robots6)

        Assert.True(symmetricAboutMiddle (3,4) robots)
        printfn "symmetric"
        printBoard (3,4) robots

        Assert.False(symmetricAboutMiddle (5,2) robots)
        printfn "notsymmetric"
        printBoard (5,2) robots

        Assert.False(symmetricAboutMiddle (5,2) robots2)
        printfn "notsymmetric"
        printBoard (5,2) robots2

        Assert.True(symmetricAboutMiddle (3,2) robots)
        printfn "symmetric"
        printBoard (3,2) robots

        Assert.True(symmetricAboutMiddle (5,2) robots3)
        printfn "symmatric"
        printBoard (5,2) robots3


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
//        let (tree,i) =simUntilXmasTree board1 robots 0 10000000
//        printfn ("%A %A") tree i

    [<Fact>]
    let testRobotReal () =
        let robots = readInit "input2.txt" 
        let afterRound100 = simRounds board2 robots 100
        Assert.Equal(231019008 , quadrantProduct board2 afterRound100)

  // idea run backwards from a xmas tree
  // we do not which robots are in which position, so not possible
  // the system is not inversible when we do not know the speeds, which robot is where

module Program = 
    open Input
    open Game
    let [<EntryPoint>] main _ = 
       // let robots1 = readInit "input1.txt" 
       // let (tree1,i1) =simUntilXmasTree board1 robots1 0 100000000
       // printfn ("%A %A") tree1 i1
        
        let robots = readInit "input2.txt" 
        let (tree,i) =simUntilXmasTree board2 robots 0 100000000
        printBoard board2 tree |> ignore
        printfn ("%A %A") tree i
        0