open System
open Xunit
module Game =
    open System.IO
    // http://www.sokobano.de/wiki/index.php?title=Level_format
    [<Literal>]
    let Wall = '#'
    [<Literal>]
    let Player = '@'
    [<Literal>]
    let Box = 'O'
    [<Literal>]
    let LeftBox = '['
    [<Literal>]
    let RightBox= ']'
    [<Literal>]
    let Floor = '.'

    [<Literal>]
    let Keypress_left = '<'
    [<Literal>]
    let Keypress_down = 'v'
    [<Literal>]
    let Keypress_up = '^'
    [<Literal>]
    let Keypress_right = '>'


    let readInit (filePath: string): string*string= 
        let txt = File.ReadAllText(filePath) 
        let lines = txt.Split([|Environment.NewLine+Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
        let moves = lines.[1].Replace(Environment.NewLine,"")
        lines.[0],moves

    type Side = Left | Right
    type Pos = Side * int 

    // y is first index , growing down on the board
    // x is index in array, growing to the right 
    type Board = Map<(Pos*int),char> 

    let comparePositions ((((s1,x1),_),_): (Pos*int)*Char) ((((_,x2),_),_): (Pos*int)*Char) =
        if (x1 > x2) then 
            1
        elif (x1 < x2) then 
            -1
        elif (s1 = Right) then
           1
        else
           -1 

    let serializeBoard (board: Board) : string =
        board |> Map.toList  
              |> List.groupBy (fun ((x,y),z) -> y) 
              |> List.sortBy (fun (y,x) -> y )
              |> List.map (fun (_,x) -> x |> List.sortWith comparePositions)
              |> List.map (List.map (fun (((side,x),y),c) -> match c with 
                                                                            | Wall -> [|Wall|]
                                                                            | RightBox -> [|']';|]
                                                                            | LeftBox-> [|'[';|]
                                                                            | Player -> [|Player|]
                                                                            | Floor-> [|'.'|]
                                                                            | _ -> [|' '|] ) ) 
              |> List.map (Array.ofList)
                      |> List.map (fun x -> String( Array.concat x ))
                      |> String.concat Environment.NewLine

    let init (board_nr:int): string*string = 
        let board1t,moves1r = readInit "input1.txt"
        let board2t,moves2r = readInit "input2.txt"
        let board3t,moves3r = readInit "input3.txt"
        let board4t,moves4r = readInit "input4.txt"

        let board: string = match board_nr with
                                | 1 -> board1t 
                                | 2 -> board2t
                                | 3 -> board3t
                                | 4 -> board4t
                                | _ -> failwith "I don't have that"
        let moves : string = match board_nr with
                                | 1 ->moves1r 
                                | 2 -> moves2r 
                                | 3 -> moves3r 
                                | 4 -> moves4r 
                                | _ -> failwith "I don't have that"

        board, moves 

    let toNewMap ((x,y): int*int) (tile: Char) : ((Pos*int)*Char) list =
        match tile with
            | Wall -> [ ((Left,x),y), Wall; ((Right ,x),y), Wall ] 
            | Player -> [((Left,x),y), Player ; ((Right ,x),y), Floor]
            | Box ->  [ ((Left,x),y), LeftBox; ((Right ,x),y), RightBox] 
            | Floor ->  [ ((Left,x),y), Floor; ((Right ,x),y), Floor ] 
            | _ -> failwith "Unknown tile"

    let parseBoard (board:string): Board = 
        board.Split(Environment.NewLine.ToCharArray()) 
                |> Array.toList
                |> List.map (fun l -> l.ToCharArray() |> Array.toList)
                |> List.filter (fun l -> l <> [])
                |> List.mapi (fun y e -> (y,e))
                |> List.map (fun (y,e) -> e |> List.mapi (fun x c -> (x,y),c))
                |> List.collect (id)
                |> List.map (fun (p,c) -> toNewMap p c)
                |> List.collect (id)
                |> Map

    let getPlayerPosition (board: Board): Pos*int =
          board |> Map.filter (fun _ t -> t=Player) |> Map.keys |> Seq.head 

    let getTile (board: Board) (((side,x),y): Pos*int): Char option = 
        Map.tryFind ((side,x),y) board

    let calcMove (((side,x),y): Pos*int) ((Δx,Δy): int*int): Pos*int = 
        match side with
            | Left when Δx = -1 -> (Right, (x-1)),y
            | Left when Δx = 1 -> (Right, (x)),y
            | Right when Δx = 1 -> (Left, (x+1)),y
            | Right when Δx = -1 -> (Left, (x)),y
            | side -> (side,x), y + Δy

    let getBehindBoxAndPosition (board: Board) (pos: Pos*int) ((Δx,Δy): int*int): (char * (Pos*int)) =
        // need to use positions 
        let rec getUntilNotBox (nextPos: Pos*int) : (char * (Pos*int)) =
            // need to check or ignore sides....
            let nextMove = calcMove nextPos (Δx,Δy) 
            let tile = getTile board nextMove 
            match tile with 
                | Some LeftBox -> getUntilNotBox nextMove 
                | Some RightBox -> getUntilNotBox nextMove 
                | Some tile -> tile, nextMove
                | None -> Wall, nextMove// think this is only for tests when there are no wall, return wallfailwith "not sure"
        getUntilNotBox pos 

    let collectAllBoxes (board: Board) (pos: Pos*int) ((Δx,Δy): int*int): (char* (Pos*int)) list =
        let rec collectBoxes (nextPos: Pos*int) : (char* (Pos*int)) list =
            let nextMove = calcMove nextPos (Δx,Δy) 
            let tile = getTile board nextMove 
            // try to handle pushing horizontally first by collecting boxes, then moving on to the other one
            if Δy = 0 then
                match tile with
                    | Some tile when tile = LeftBox || tile = RightBox -> (tile, nextMove)  ::  collectBoxes nextMove
                    | Some tile -> []//when tile = Floor -> [] 
                    | None -> failwith "out of board without hitting wall"
            else 
                match tile with 
                    | Some LeftBox -> 
                            let rightPos = calcMove nextMove (1, 0)
                            let newBoxes = collectBoxes rightPos 
                            (LeftBox, nextMove) :: (RightBox,rightPos ):: collectBoxes nextMove @ newBoxes
                    | Some RightBox -> 
                            let leftPos = calcMove nextMove (-1, 0)
                            let newBoxes = collectBoxes leftPos 
                            (RightBox, nextMove) :: (LeftBox, leftPos) ::collectBoxes nextMove @ newBoxes
                    | Some tile -> []
                    | None -> failwith "out of board without hitting wall" 
        collectBoxes pos 


    let canPushBox (board: Board) ((x,y): Pos*int) ((Δx,Δy): int*int): bool = 
        // need to find the first tile that is not a box after all boxes in a 
        // row of boxes and if that is floor , then we push
        let collectAllBoxes = collectAllBoxes board (x,y) (Δx,Δy)
        let tilesBehindBoxes = collectAllBoxes |> List.map (fun (_,box) -> getBehindBoxAndPosition board box (Δx,Δy))
                                              |> List.map (fun (tile,_) -> tile)
        tilesBehindBoxes |> List.forall (fun tileBehindBoxes -> tileBehindBoxes = Floor)

    [<Fact>]
    let testPushBoxes() = 
        let board = Map.empty |> Map.add ((Left,0),0) Player |> Map.add ((Right,0) ,0) LeftBox |> Map.add ((Left,1),0) Floor
        printfn "%s" (serializeBoard board)
        Assert.True(canPushBox board ((Left,0),0) (1,0))
 //       Assert.False(canPushBox board ((Left,0),0) (-1,0))
 //       Assert.False(canPushBox board ((Left,0),0) (0,1))

    [<Fact>]
    let testPushMultipleBoxes() = 
        let board = Map.empty |> Map.add ((Left,0),0) Player |> Map.add ((Right,0),0) LeftBox |> Map.add ((Left,1),0) RightBox |> Map.add ((Right,1),0) LeftBox |> Map.add ((Left,2),0) Floor |> Map.add ((Right,2),0) Wall
        printfn "%s" (serializeBoard board)
        Assert.True(canPushBox board ((Left,0),0) (1,0))
//        Assert.False(canPushBox board ((Left,0),0) (-1,0))
//        Assert.False(canPushBox board ((Left,0),0) (0,1))

    [<Fact>]
    let testPushMultipleBoxesWithWall() = 
        let board = Map.empty |> Map.add ((Left,0),0) Player |> Map.add ((Right,0),0) LeftBox |> Map.add ((Left,1),0) LeftBox |> Map.add ((Right,1),0) LeftBox |> Map.add ((Left,2),0) Wall 
        printfn "%s" (serializeBoard board)
        Assert.False(canPushBox board ((Left,0),0) (1,0))

    let legalMove (board: Board) (Δ: int*int): bool = 
        let (Δx,Δy) = Δ
        let playerPos = getPlayerPosition board
        let pos' = calcMove playerPos Δ 
        let t' = getTile board pos' 
        printfn "checking move %A " pos'
        match t' with
            | Some c when c = Wall -> false 
            | Some c when c = LeftBox || c = RightBox-> canPushBox board playerPos (Δx,Δy) 
            | Some _ -> true
            | None -> false

    let updatedMap elementsToAdd initialMap = 
        elementsToAdd |> List.fold (fun acc (key, value) -> Map.add key value acc) initialMap

    let move (board: Board) ((Δx,Δy): int*int): Board = 
        let playerPos = getPlayerPosition board
        let ((side, x),y) = playerPos 
        let pos' = calcMove playerPos (Δx,Δy)
        let tile_Δ = getTile board pos' 
        match tile_Δ with
            | Some Floor -> board
                            |> Map.remove playerPos 
                            |> Map.remove pos' 
                            |> Map.add pos' Player  
                            |> Map.add playerPos Floor 
            | Some LeftBox | Some RightBox ->
                        let boxes = collectAllBoxes board playerPos (Δx,Δy)
                        let oldBoxesPositions = boxes |> List.map (fun (_,box) -> box)
                        let moveBoxes = boxes |> List.map (fun (c,pos) -> calcMove pos (Δx,Δy), c) // move different for right and leftboxes//|> List.map (fun x,p -> )
                        // can remove this was not needed
                        board 
                            |> Map.remove playerPos 
                            |> Map.filter (fun x _ -> oldBoxesPositions |> List.contains x |> not) 
                            |> (updatedMap moveBoxes)
                            |> Map.add pos' Player
                            |> Map.add playerPos Floor
            | _ -> board

    [<Fact>]
    let testPushMultipleBoxesBoard() = 
        let board = Map.empty |> Map.add ((Left,0),0) Player |> Map.add ((Right,0),0) LeftBox |> Map.add ((Left,1),0) RightBox |> Map.add ((Right,1),0) LeftBox |> Map.add ((Left,2),0) RightBox |> Map.add ((Right,2),0) Floor |> Map.add ((Left,3),0) Wall
        let boardAfterPush = Map.empty |> Map.add ((Left,0),0) Floor |> Map.add ((Right,0),0) Player |> Map.add ((Left,1),0) LeftBox |> Map.add ((Right,1),0) RightBox |> Map.add ((Left,2),0) LeftBox |> Map.add ((Right,2),0) RightBox  |> Map.add ((Left,3),0) Wall
        printfn "board %s" (serializeBoard board)
        printfn "fact %s" (serializeBoard boardAfterPush)
        Assert.True(canPushBox board ((Left,0),0) (1,0))
        let push = move board (1,0)
        printfn "calculation %s" (serializeBoard push) 
        Assert.Equivalent(boardAfterPush, push)

    let movePlayer (board: Board) (keypress: Char): Board = 
        let Δ = match keypress with
                                | Keypress_left -> (-1,0) 
                                | Keypress_down -> (0,1) 
                                | Keypress_right -> (1,0) 
                                | Keypress_up -> (0,-1) 
                                | _ -> failwith "There are only four known directions." 
        if (legalMove board Δ) then
            printfn "legal move"
            move board Δ
        else 
            printfn "illegal move"
            board//

    let playBoard (boardnr: int): Board = 
        let boardS,allMoves = init boardnr
        let board = parseBoard boardS
        let rec playAllMoves (board: Board) (moves: string) =
            printfn "%s" (serializeBoard board)
            let move = moves |> Seq.tryHead 
            match move with
                | Some m -> let board' = movePlayer board m
                            printfn "Move %A" m 
                            printfn ""
                            playAllMoves board' (String(Seq.tail moves |> Seq.toArray))
                | None -> board 
        playAllMoves board allMoves

    let sumOfBoxes (i: int): int =
        let board = playBoard i
        let boxes = board |> Map.toSeq |> Seq.filter (fun (p,c) -> c = LeftBox )  |> Seq.map (fun (p,c) -> p)
        boxes |> Seq.sumBy (fun ((_,x),y) -> x+y*100)

        (*
    [<Fact>]
    let testBoxesSum () =
        Assert.Equal(2028,sumOfBoxes 1)
        Assert.Equal(10092,sumOfBoxes 2)
       // Assert.Equal(1360570,sumOfBoxes 3)
*)
module Program = 
    open Game
    let [<EntryPoint>] main _ =
        let finishedBoard = playBoard 4
        printfn "%s" (serializeBoard finishedBoard)
        Console.ReadKey() |> ignore
        0