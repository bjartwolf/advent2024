module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): char[][] = 
        File.ReadAllLines(filePath) |> Array.map (fun x -> x.ToCharArray())

    type Position = int*int
    type Tile = Position * char
    type Region = Set<Tile>
    type Regions = Region list

    let minChar = 'A' |> int
    let maxChar = 'Z' |> int
    let scanned = 'x' |> int

    let isScanned (input: char[][]) (x:int) (y:int) : bool = 
        if x < 0 || y < 0 || x >= input.Length || y >= input.[0].Length then false
        else input.[x].[y] |> int = scanned

    let tileType (input: char[][]) (x:int) (y:int) : char option = 
        if x < 0 || y < 0 || x >= input.Length || y >= input.[0].Length then None 
        else Some input.[x].[y]  

    let isInScannedRegion (regions: Regions)  ((x,y): Position ): bool = 
        regions |> List.exists (fun region -> region |> Set.exists (fun ((x',y'),_) -> x = x' && y = y'))

    let rec findRegions (input: char[][]) : Regions = 
        let rec findRegions' (positionsToScan: Position list)  (regions: Regions) : Regions = 
            match positionsToScan with
            | pos :: restPos ->  
                    // find candidates around it
                    let (x,y) = pos 
                    let up = (x,y-1) 
                    let left = (x-1,y) 
                    let down = (x,y-1) 
                    let right = (x+1,y) 
                    let candidates = [up;left;down;right]
                    let unScannedCandidates = candidates |> List.filter (fun (x,y) -> not (isScanned input x y))
                    let tileTypeCurrent = tileType input x y 
                    match tileTypeCurrent with
                        | Some tileType -> 
                            let regionIndex = regions |> List.tryFindIndex (fun region -> region |> Set.contains (pos,tileType) )
                            match regionIndex with
                                | None -> findRegions' restPos (Set.singleton (pos, tileType) :: regions)
                                | Some i -> let newRegion =  regions.[i] |> Set.add (pos,tileType)
                                            let newRegions = regions |> List.updateAt i newRegion
                                            findRegions' restPos newRegions 
                        | None -> findRegions' restPos regions 
            | [] -> regions
                 

        let size = input.Length
        let xs = [0..size-1]
        let ys = [0..size-1]
        let positions = [for x in xs do for y in ys do yield (x,y)]

        findRegions' positions []
    // areal og omkrets

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
//        printfn "%A" input
        let findRegions = findRegions input
        printfn "%A" findRegions
        Assert.Equal(10, input.Length) 

module Program = let [<EntryPoint>] main _ = 0