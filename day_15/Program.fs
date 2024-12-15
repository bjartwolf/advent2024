open System
module Game =
    open System.IO
    open Xunit 

    let readInit (filePath: string): string*string= 
        let txt = File.ReadAllText(filePath) 
        let lines = txt.Split([|Environment.NewLine+Environment.NewLine|], StringSplitOptions.RemoveEmptyEntries)
        lines.[0],lines.[1]

    // y is first index , growing down on the board
    // x is index in array, growing to the right 
    type Board = Map<(int*int),char> 

    let init (board_nr:int): String = 
        let board1t,moves1r = readInit "input1.txt"
        let board2t,moves2r = readInit "input2.txt"

        let board: string = match board_nr with
                                | 1 -> board1t 
                                | 2 -> board2t
                                | _ -> failwith "I don't have that"
        let removeFirstNewline = board |> Seq.skip (Environment.NewLine.Length) |> Seq.toArray
        String(removeFirstNewline)


    // http://www.sokobano.de/wiki/index.php?title=Level_format
    let wall = '#'
    let player = '@'
//    let player_on_goal_square = '+'
    let box = 'O'
//    let box_on_goal_square = '*'
 //   let goal_square = '.'
    let floor = '.'

    let keypress_left = '<'
    let keypress_down = 'v'
    let keypress_up = '^'
    let keypress_right = '>'

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
        let tileBehindBox = getTile board (x+2*Δx, y+2*Δy)
        tileBehindBox = Some floor || tileBehindBox = Some goal_square 

    let legalMove (board: Board) (Δ: int*int): bool = 
        let (Δx,Δy) = Δ
        let (x,y) = getPlayerPosition board
        let pos' = x+Δx,y+Δy
        let t' = getTile board pos' 
        match t' with
            | Some c when c = wall -> false 
            | Some c when c = box || c = box_on_goal_square -> canPushBox board (x,y) (Δx,Δy)
            | Some _ -> true
            | None -> false
        
    let move (board: Board) ((Δx,Δy): int*int): Board = 
        let (x,y) = getPlayerPosition board
        let pos' = x+Δx,y+Δy
        let tile = getTile board (x,y)
        let tile_Δ = getTile board (x+Δx,y+Δy)


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

    let movePlayer (board: Board) (keypress: Char): (Board * Char option) =
        let Δ = match keypress with
                                | 'l' -> (-1,0) 
                                | 'd' -> (0,1) 
                                | 'r' -> (1,0) 
                                | 'u' -> (0,-1) 
                                | _ -> failwith "There are only four known directions." 
        if (legalMove board Δ) then
            (move board Δ, Some keypress)
        else 
            (board, None)

    let serializeBoard (board: Board) : string =
        board |> Map.toList  
              |> List.groupBy (fun ((_,y),_) -> y) 
              |> List.map (snd) |> List.map (List.map (snd)) 
              |> List.map (Array.ofList)
              |> List.map (String)
              |> String.concat Environment.NewLine

    // I think I will deprecate this method in favor of something that takes a board nr, previous
    // moves and a new move and returns the serialized board and the set of moves.
    let makeMove(board: string, move: Char) = 
        let board = parseBoard board
        let (newBoard,_) = movePlayer board move
        serializeBoard newBoard

    let mutable allKnownBoards: Map<int*string,Board> = Map.empty

    let startNewBoard (boardnr: int): (string) = 
        let board = parseBoard (init boardnr) 
        // TBD:
        // this might give some racestuff at a server with concurrency... 
        // should likely use a memorycache or something, which is threadsafe
        allKnownBoards <- allKnownBoards.Add( (boardnr,""),board)
        serializeBoard board 

//    let wonGame (board: Board): bool =
//        not (board.Values.Contains(goal_square)) && not (board.Values.Contains(player_on_goal_square))

    let rec lookupBoard (boardnr: int) (state: string): Board =
        let b = allKnownBoards.TryFind (boardnr, state)
        match (b,state) with 
            | (Some board,_) -> board
            | (None, "")  -> parseBoard(init boardnr)
            | (None, _)  -> 
                let lastMove = state.[state.Length-1]
                let previousState = state.Remove(state.Length-1) 
                let (board,_) = movePlayer (lookupBoard boardnr previousState) lastMove 
                board 

    let rememberBoard (boardnr: int) (state: string) (board: Board) : unit =
        allKnownBoards <- allKnownBoards.Add( (boardnr,state),board)

    let attemptMove(boardnr: int, history: string, move: Char): (string*string) = 
        // lookup board from history (it should be there, how else can we actually play?)
        // it could have some fancy feature to re-build the solution, as someone could ofcourse start
        // fancy or load or something, but for now I guess it is fine to just assume we actually
        // have the previous state in our map
        // I guess we neeed to have another intiialize-function that takes no moves.
        if move = 'b' then 
            let history_undoed = if String.IsNullOrEmpty(history) then 
                                    "" 
                                 else 
                                    history.Remove(history.Length-1) 
            let board = lookupBoard boardnr history_undoed 
 //           if (wonGame board) then
 //               (serializeBoard (writeWinOnBoard board), history_undoed)
 //           else 
            (serializeBoard board, history_undoed)
        else 
            let board = lookupBoard boardnr history 
            let (board', moveMade) = movePlayer board move

            let history' =  history + match moveMade with 
                                        | Some move -> move.ToString()
                                        | None -> ""
            rememberBoard boardnr history' board' 
            //if (wonGame board') then
            //    (serializeBoard (writeWinOnBoard board'), history')
            //else 
            (serializeBoard board', history')


//    [<Fact>]
//    let test2 () = 

        //let input = readInit "input1.txt" 
//        Assert.Equal(1, input.Length) 

module Program = 
    let [<EntryPoint>] main _ =
        0