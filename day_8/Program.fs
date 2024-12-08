module Input =
    open System
    open System.IO
    open Xunit 

    type Position = int * int
    type Tile = char option 
    type World = Map<Position, Tile>

    let readInit (filePath: string): string [] = 
        IO.File.ReadAllLines(filePath)

    let parsePipeMap (mapinput: string[]): World =
        [
            for i in 0 .. (mapinput.Length - 1) do
                let chars = mapinput[i].ToCharArray()
                for charIndex in 0 .. (chars.Length - 1) do
                    let char = chars[charIndex]
                    match char with 
                          | '.' -> yield  ((i,charIndex),None)
                          | antenna -> yield  ((i,charIndex), Some antenna)
        ] |> Map.ofList

    let findAntennas (world: World): char seq = 
        world |> Map.toSeq
              |> Seq.filter (fun (pos,tile) -> match tile with | Some c -> true | _ -> false) 
              |> Seq.map (fun (pos,tile) -> match tile with | Some c -> c | _ -> failwith "should only be antennas") 

    let findAntennasOfType (antenna: char) (world: World) : Position seq = 
        world |> Map.toSeq
              |> Seq.filter (fun (pos,tile) -> match tile with | Some c -> c = antenna| _ -> false) 
              |> Seq.map (fun (pos,tile) -> match tile with | Some c -> pos | _ -> failwith "should only be antennas") 


    let findAntennaTypes (world: World): char list = 
        world |> findAntennas 
              |> Seq.distinct
              |> Seq.toList

    let findAllPairs (input: Position list): (Position*Position) list =
        List.allPairs input input
            |> List.filter (fun ((pos1,pos2)) -> pos1 <> pos2)
            |> List.distinctBy (fun (pos1,pos2) -> if pos1 < pos2 then (pos1,pos2) else (pos2,pos1))

    let findAllAntennaLines (world: World): (Position*Position) seq  = 
        let types = world |> findAntennaTypes
        [
            for antennaType in types do
                let positions = findAntennasOfType antennaType world
                let pairs = findAllPairs (Seq.toList positions)
                yield! pairs 
        ]

        
    let findAntiNodes (((x1,y1),(x2,y2)): (Position*Position)): Position seq =
        [ let deltaX = x2 - x1 
          let deltaY = y2 - y1
          yield (x1 - deltaX, y1 - deltaY)
          yield (x2 + deltaX, y2 + deltaY) ]

    let filterAntiNodesOutsideWorld (n: int) (nodes: Position seq) : Position seq=
        nodes |> Seq.filter (fun (x,y) -> x >= 0 && x < n && y >= 0 && y < n)

    let countAntiNodes (world: World) (worldSize: int):int =
        let lines = findAllAntennaLines world 
        let antiNodes = lines |> Seq.map findAntiNodes |> Seq.concat
        let nodesInThisWorld = filterAntiNodesOutsideWorld worldSize antiNodes 
        nodesInThisWorld |> Seq.distinct |> Seq.length

    let printWorld (n: int) (world: World) = 
        for i in 0 .. n - 1 do
            for j in 0 .. n - 1 do
                match world.TryFind((i,j)) with
                | Some None -> printf "."
                | Some (Some a) -> printf "%c" a
                | None -> failwith "oops" 
            printfn ""

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(12, input.Length) 
        let map = parsePipeMap input
        printWorld input.Length map
        Assert.Equal(12*12, map.Count) 
        Assert.Equal(2, map |> findAntennaTypes |> List.length) 
        printfn "%A" (findAllAntennaLines map)
        Assert.Equal(9, map |> findAllAntennaLines |> Seq.length) 
        Assert.Contains('A', map |> findAntennaTypes) 
        Assert.Contains('0', map |> findAntennaTypes) 
        Assert.Equal(3, map |> findAntennasOfType 'A' |> Seq.length) 
        Assert.Equal(14, countAntiNodes map input.Length )

    [<Fact>]
    let test3 () = 
        let input = readInit "input2.txt" 
        let map = parsePipeMap input
        Assert.Equal(252, countAntiNodes map input.Length )

module Program = let [<EntryPoint>] main _ = 0