open System
open Xunit
module Game =
    open System.IO

    let readInit (filePath: string): string*string= 
        let txt = File.ReadAllText(filePath) 
        let lines = txt.Split([|Environment.NewLine+Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
        let moves = lines.[1].Replace(Environment.NewLine,"")
        lines.[0],moves

    // y is first index , growing down on the board
    // x is index in array, growing to the right 
    type Board = Map<(int*int),char> 

    let serializeBoard (board: Board) : string =
        board |> Map.toList  
              |> List.groupBy (fun ((_,y),_) -> y) 
              |> List.map (snd) |> List.map (List.map (snd)) 
              |> List.map (Array.ofList)
              |> List.map (String)
              |> String.concat Environment.NewLine

    let init (board_nr:int): string*string = 
        let board1t,moves1r = readInit "input1.txt"
        let board2t,moves2r = readInit "input2.txt"

        let board: string = match board_nr with
                                | 1 -> board1t 
                                | 2 -> board2t
                                | _ -> failwith "I don't have that"
        let moves : string = match board_nr with
                                | 1 ->moves1r 
                                | 2 -> moves2r 
                                | _ -> failwith "I don't have that"

        board, moves 
//        let removeFirstNewline = board |> Seq.skip (Environment.NewLine.Length) |> Seq.toArray
//        String(removeFirstNewline)


    // http://www.sokobano.de/wiki/index.php?title=Level_format
    [<Literal>]
    let Wall = '#'
    [<Literal>]
    let Player = '@'
//    let player_on_goal_square = '+'
    [<Literal>]
    let Box = 'O'
//    let box_on_goal_square = '*'
 //   let goal_square = '.'
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

    let parseBoard (board:string): Board = 
        board.Split(Environment.NewLine.ToCharArray()) 
            |> Array.toList
            |> List.map (fun l -> l.ToCharArray() |> Array.toList)
            |> List.filter (fun l -> l <> [])
            |> List.mapi (fun y e -> (y,e))
            |> List.map (fun (y,e) -> e |> List.mapi (fun x c -> (x,y),c))
            |> List.collect (id)
            |> Map

    let getPlayerPosition (board: Board): int*int =
          //board |> Map.filter (fun _ t -> t=player || t=player_on_goal_square )|> Map.keys |> Seq.head 
          board |> Map.filter (fun _ t -> t=Player) |> Map.keys |> Seq.head 

    let getTile (board: Board) (pos: int*int): Char option = Map.tryFind pos board

    let getBehindBoxAndPosition (board: Board) ((x,y): int*int) ((Δx,Δy): int*int): (char * int) =
        let rec getUntilNotBox (i: int) : (char * int) =
            let tile = getTile board (x+i*Δx,y+i*Δy)
            match tile with 
                | Some Box -> getUntilNotBox (i+1) 
                | Some tile -> tile, i
                | None -> Wall, i// think this is only for tests when there are no wall, return wallfailwith "not sure"
        getUntilNotBox 1 

    let canPushBox (board: Board) ((x,y): int*int) ((Δx,Δy): int*int): bool = 
        // need to find the first tile that is not a box after all boxes in a 
        // row of boxes and if that is floor , then we push
        let tileBehindBoxes = getBehindBoxAndPosition board (x,y) (Δx,Δy)
        match tileBehindBoxes with
            | Floor, _ -> true
            | _ -> false

        // ikke skriv regler som ikke er lov i tullesokoban, det blir bare pes.
    [<Fact>]
    let testPushBoxes() = 
        let board = Map.empty |> Map.add (0,0) Player |> Map.add (1,0) Box |> Map.add (2,0) Floor
        printfn "%s" (serializeBoard board)
        Assert.True(canPushBox board (0,0) (1,0))
        Assert.False(canPushBox board (0,0) (-1,0))
        Assert.False(canPushBox board (0,0) (0,1))

    [<Fact>]
    let testPushMultipleBoxes() = 
        let board = Map.empty |> Map.add (0,0) Player |> Map.add (1,0) Box |> Map.add (2,0) Box |> Map.add (3,0) Box |> Map.add (4,0) Floor
        printfn "%s" (serializeBoard board)
        Assert.True(canPushBox board (0,0) (1,0))
        Assert.False(canPushBox board (0,0) (-1,0))
        Assert.False(canPushBox board (0,0) (0,1))

    [<Fact>]
    let testPushMultipleBoxesWithWall() = 
        let board = Map.empty |> Map.add (0,0) Player |> Map.add (1,0) Box |> Map.add (2,0) Box |> Map.add (3,0) Box |> Map.add (4,0) Wall 
        Assert.False(canPushBox board (0,0) (1,0))


    let legalMove (board: Board) (Δ: int*int): bool = 
        let (Δx,Δy) = Δ
        let (x,y) = getPlayerPosition board
        let pos' = x+Δx,y+Δy
        let t' = getTile board pos' 
        match t' with
            | Some c when c = Wall -> false 
            | Some c when c = Box -> canPushBox board (x,y) (Δx,Δy) 
            | Some _ -> true
            | None -> false
        
    let move (board: Board) ((Δx,Δy): int*int): Board = 
        let (x,y) = getPlayerPosition board
        let pos' = x+Δx,y+Δy
       // let tile = getTile board (x,y)
        let tile_Δ = getTile board (x+Δx,y+Δy)
        match tile_Δ with
            | Some Floor -> board
                            |> Map.remove (x,y) 
                            |> Map.add pos' Player  
                            |> Map.add (x,y) Floor
            | Some Box ->
                        let (tileBehind, i) = getBehindBoxAndPosition board (x,y) (Δx,Δy)
                        let lastBoxPosition = i - 1
//                        printfn " last box position %A" lastBoxPosition
//                        printfn " i %A" i 
                        if tileBehind <> Floor then failwith "this was not a legal move"
                        board 
                            |> Map.remove (x,y) 
                            |> Map.remove pos' 
                            |> Map.remove (x+2*Δx,y+2*Δy) 
                            |> Map.add (x,y) Floor
                            |> Map.add pos' Player  
                            |> Map.add (x+2*Δx,y+2*Δy) Box
                            |> Map.add (x+i*Δx,y+i*Δy) Box
            | _ -> board

    [<Fact>]
    let testPushMultipleBoxesBoard() = 
        let board = Map.empty |> Map.add (0,0) Player |> Map.add (1,0) Box |> Map.add (2,0) Box |> Map.add (3,0) Box |> Map.add (4,0) Floor
        let boardAfterPush = Map.empty |> Map.add (0,0) Floor |> Map.add (1,0) Player |> Map.add (2,0) Box |> Map.add (3,0) Box |> Map.add (4,0) Box 
        printfn "%s" (serializeBoard board)
        printfn "%s" (serializeBoard boardAfterPush)
        let push = move board (1,0)
        printfn "%s" (serializeBoard push) 
        Assert.Equivalent(boardAfterPush, push)



        (*
        let tile_Δ' = if tile_Δ = Some goal_square || tile_Δ= Some box_on_goal_square then
                                     player_on_goal_square 
                                  else 
                                     player
        let isPushingBox =  tile_Δ = Some box || tile_Δ = Some box_on_goal_square 

        let whatWasUnderPlayer = if tile = Some player then floor else goal_square
        let boardWithoutPlayer = board |> Map.add (x,y) whatWasUnderPlayer
        let boardWithPlayerBack = boardWithoutPlayer |> Map.add pos' tile_Δ'

        if isPushingBox then 
            let isBoxPushedOnGoalSquare = getTile board (x+2*Δx,y+2*Δy) = Some goal_square 
            let boxTile = if isBoxPushedOnGoalSquare then box_on_goal_square else box
            boardWithPlayerBack |> Map.add (x+2*Δx,y+2*Δy) boxTile
        else 
            boardWithPlayerBack 
*)
// todo add rules

    //let movePlayer (board: Board) (keypress: Char): (Board * Char option) =
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
//            (move board Δ, Some keypress)
        else 
            printfn "illegal move"
            board//
            //(board, None)

    let playBoard (boardnr: int): Board = 
        let boardS,allMoves = init boardnr
        let board = parseBoard boardS
        let rec playAllMoves (board: Board) (moves: string) =
            let move = moves |> Seq.tryHead 
            match move with
                | Some m -> let board' = movePlayer board m
                            printfn "Move %A" m 
                            printfn "%s" (serializeBoard board')
                            printfn ""
                            playAllMoves board' (String(Seq.tail moves |> Seq.toArray))
                | None -> board 
        playAllMoves board allMoves




//    [<Fact>]
//    let test2 () = 

        //let input = readInit "input1.txt" 
//        Assert.Equal(1, input.Length) 

module Program = 
    open Game
    let [<EntryPoint>] main _ =
        let finishedBoard = playBoard 1
        printfn "%s" (serializeBoard finishedBoard)
        Console.ReadKey() |> ignore
        0