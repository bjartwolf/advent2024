open System
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
    let wall = '#'
    let player = '@'
//    let player_on_goal_square = '+'
    let box = 'O'
//    let box_on_goal_square = '*'
 //   let goal_square = '.'
    let floor = '.'

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
          board |> Map.filter (fun _ t -> t=player) |> Map.keys |> Seq.head 

    let getTile (board: Board) (pos: int*int): Char option = Map.tryFind pos board

    let canPushBox (board: Board) ((x,y): int*int) ((Δx,Δy): int*int): bool = 
        // this needs to change later
        let tileBehindBox = getTile board (x+2*Δx, y+2*Δy)
        tileBehindBox = Some floor //|| tileBehindBox = Some goal_square 

    let legalMove (board: Board) (Δ: int*int): bool = 
        let (Δx,Δy) = Δ
        let (x,y) = getPlayerPosition board
        let pos' = x+Δx,y+Δy
        let t' = getTile board pos' 
        match t' with
            | Some c when c = wall -> false 
            | Some c when c = box -> canPushBox board (x,y) (Δx,Δy) // TODO PUSH ROW OF BOXES
            //| Some c when c = box || c = box_on_goal_square -> canPushBox board (x,y) (Δx,Δy)
            | Some _ -> true
            | None -> false
        
    let move (board: Board) ((Δx,Δy): int*int): Board = 
        let (x,y) = getPlayerPosition board
        let pos' = x+Δx,y+Δy
       // let tile = getTile board (x,y)
        let tile_Δ = getTile board (x+Δx,y+Δy)
        match tile_Δ with
            | Some floor -> board |> Map.remove (x,y) |> Map.add pos' player  |> Map.add (x,y) floor
            | _ -> board

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
            move board Δ
//            (move board Δ, Some keypress)
        else 
            board//
            //(board, None)

    let serializeBoard (board: Board) : string =
        board |> Map.toList  
              |> List.groupBy (fun ((_,y),_) -> y) 
              |> List.map (snd) |> List.map (List.map (snd)) 
              |> List.map (Array.ofList)
              |> List.map (String)
              |> String.concat Environment.NewLine

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