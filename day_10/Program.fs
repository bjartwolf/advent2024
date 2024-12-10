module Input =
    open System
    open System.IO
    open Xunit 

    type Position = int * int
    type Kart = Map<Position, int>
    type Walk = Position list
    type Trailheads = Position list
    type Direction = W | N | E | S 
    let directions = [W; N; E; S]

    let candidateMoves (kart: Kart) ((x,y): Position): Position list =
        directions |> List.map (fun direction -> 
            match direction with 
            | N -> (x-1,y)
            | W -> (x,y-1)
            | E -> (x,y+1)
            | S -> (x+1,y)
        ) |> List.filter (fun (x,y) -> Map.containsKey (x,y) kart)
        
    let readInit (filePath: string): string[] = File.ReadAllLines(filePath)
    let parseInit (input: string[]) : Kart*Trailheads = 
        let foo: (Position*int) list = [
            for i in 0 .. (input.Length - 1) do
                let chars = input[i].ToCharArray()
                for charIndex in 0 .. (chars.Length - 1) do
                    let char = chars[charIndex]
                    yield ((i,charIndex), Int32.Parse(char.ToString()))
        ] 
        let trailHeads = [
            for i in 0 .. (input.Length - 1) do
                let chars = input[i].ToCharArray()
                for charIndex in 0 .. (chars.Length - 1) do
                    let char = chars[charIndex]
                    if char = '0' then yield (i, charIndex)
        ]
        foo |> Map.ofList, trailHeads

    let walkFromTrailHead (kart: Kart) (trailHead: Position) : int = 
        printfn "walk from %A" trailHead
        let rec walk (height:int) (pos: Position): int= 
            if height = 9 then
                printfn "GOAL %A" pos
                1
            else 
                let moves = candidateMoves kart pos 
                let legalMoves = moves |> List.filter (fun pos -> (Map.find pos kart) = height + 1)
                printfn "LEGALMOVES %A %A" legalMoves height
                if legalMoves = [] then 0 
                // check all legal moves recusrively
                else legalMoves |> List.sumBy (fun move -> walk (height + 1) move ) 
        walk 0 trailHead

    let uniqueTopsFromTrailHead (kart: Kart) (trailHead: Position) : int = 
        let mutable tops: Set<Position> = Set.empty
        printfn "walk from %A" trailHead
        let rec walk (height:int) (pos: Position): int= 
            if height = 9 then
                if not (tops.Contains pos) then
                    printfn "GOAL %A" pos
                    tops <- tops.Add pos
                    1
                else 
                    0
            else 
                let moves = candidateMoves kart pos 
                let legalMoves = moves |> List.filter (fun pos -> (Map.find pos kart) = height + 1)
                printfn "LEGALMOVES %A %A" legalMoves height
                if legalMoves = [] then 0 
                // check all legal moves recusrively
                else legalMoves |> List.sumBy (fun move -> walk (height + 1) move ) 
        walk 0 trailHead


//    let walksThatCount (walks: Walk list): Walk list =
 //       walks |> List.filter (fun w -> w |> List.length= 10) 

    let walkFromTrailheads (kart: Kart) (trailHeads: Trailheads): int list = 
        let trails = trailHeads |> List.map (fun trailHead -> walkFromTrailHead kart trailHead) 
                     //           |> walksThatCount 
        [0]

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(8, input.Length) 
        let mapInput = [|"003"; "456"; "709"|]
        let map,trailHeads  = parseInit mapInput
        Assert.Equal(5, Map.find (1,1) map)
        Assert.Equal(9, Map.find (2,2) map)
        Assert.Equivalent([(0,0);(0,1);(2,1)], trailHeads)
        Assert.Equivalent([(0,1);(1,0);(1,2);(2,1)], candidateMoves map (1,1))
        Assert.Equivalent([(0,1);(1,0)], candidateMoves map (0,0))

        let map,trailHeads  = parseInit input 
        Assert.Equal(9, trailHeads |> List.length)
        Assert.Equal(5, trailHeads.[0] |> uniqueTopsFromTrailHead map)
        Assert.Equal(36, trailHeads |> List.map (fun w -> uniqueTopsFromTrailHead map w)|> List.sum)
        Assert.Equal(81, trailHeads |> List.map (fun w -> walkFromTrailHead map w)|> List.sum)
        let input = readInit "input2.txt" 
        let map,trailHeads  = parseInit input 
        Assert.Equal(510, trailHeads |> List.map (fun w -> uniqueTopsFromTrailHead map w)|> List.sum)
        Assert.Equal(1058, trailHeads |> List.map (fun w -> walkFromTrailHead map w)|> List.sum)

module Program = let [<EntryPoint>] main _ = 0