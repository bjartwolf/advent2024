module Input =
    open System.IO
    open Xunit 

    let readInit (filePath: string): char[][] = 
        File.ReadAllLines(filePath) |> Array.map (fun x -> x.ToCharArray())

    type Position = int*int
    type Tile = Position * char
    type Region = Set<Tile>
    type Regions = Region list

    let tileType (input: char[][]) (x:int) (y:int) : char option = 
        if x < 0 || y < 0 || x >= input.Length || y >= input.[0].Length then None 
        else Some input.[x].[y]  

    let findCandidates (pos: Position): Position list =
        let (x,y) = pos 
        let up = (x,y-1) 
        let left = (x-1,y) 
        let down = (x,y+1) 
        let right = (x+1,y) 
        [up;left;down;right]

    let rec findRegions (input: char[][]) : Regions = 
        let rec findRegions' (positionsToScan: Position list)  (regions: Regions) : Regions = 
            match positionsToScan with
            | pos :: restPos ->  
                    // find candidates around it
                    let (x,y) = pos 
                    let candidates = findCandidates pos 
//                    let unScannedCandidates = candidates |> List.filter (fun (x,y) -> not (isScanned input x y))
                    let tileTypeCurrent = tileType input x y 
                    match tileTypeCurrent with
                        | Some tileType -> 
                            let regionIndex c = regions |> List.tryFindIndex (fun region -> region |> Set.contains (c,tileType) ) // check this for all candiadates
                            let cs = candidates |> List.map (fun c -> regionIndex c) |> List.choose id |> List.distinct
                            match cs with
                                | []-> findRegions' restPos (Set.singleton (pos, tileType) :: regions)
                                | i::j:: _ -> // ouch, two regions. must merge then, remove the other from the list
                                              let mergedRegions = Set.union regions.[i] regions.[j] 
                                              let newRegions = regions |> List.updateAt i mergedRegions
                                              let removedRegions = newRegions |> List.removeAt j 
                                              findRegions' (pos :: restPos) removedRegions 
                                | i:: _ -> let newRegion =  regions.[i] |> Set.add (pos,tileType)
                                           let newRegions = regions |> List.updateAt i newRegion
                                           findRegions' restPos newRegions 
            | [] -> regions
                 
        let size = input.Length
        let xs = [0..size-1]
        let ys = [0..size-1]
        let positions = [for x in xs do for y in ys do yield (x,y)]

        findRegions' positions []

    let findNeighbors (region: Region): Position list =
        region 
            |> Set.map (fun (p,_) -> findCandidates p |> Set.ofList) 
            |> Set.map (fun x -> x |> Set.toList) 
            |> Set.toList 
            |> List.collect id

    let findNeighborPositions (region: Region): Position list =
        let positionsInSet = region |> Set.map (fun (p,t) -> p) |> Set.toList
        let neighbors = findNeighbors region
        neighbors |> List.filter (fun x -> not (positionsInSet |> List.exists (fun y -> y = x)))

    let findNeighborCount (region: Region): int =
        findNeighborPositions region
            |> List.length

    let cost (region: Region): int = 
        (findNeighborCount region) * (Set.count region)

    let isCornerInRegion (t:Tile) (region:Region) (a: int) (b:int): bool =
        let (x,y),c = t
        let N = (x+a,y) 
        let W = (x,y+b) 
        let NW = (x+a,y+b) 
        if (Set.contains (N,c) region) && (Set.contains (W,c) region) && not (Set.contains (NW,c) region)  then 
            true 
        elif not (Set.contains (N,c) region) && not(Set.contains (W,c) region) then
            true 
        else
            false

    let countInRegionFor (t:Tile) (region:Region): int =
        let a = if (isCornerInRegion t region 1 -1) then 1 else 0
        let b = if (isCornerInRegion t region -1 -1) then 1 else 0
        let c = if (isCornerInRegion t region -1 1) then 1 else 0
        let d = if (isCornerInRegion t region 1 1) then 1 else 0
        a + b + c + d

    let findNrOfCorners (region: Region): int = 
        region |> Set.toSeq |> Seq.map (fun t -> countInRegionFor t region) |> Seq.sum 

    let cost2 (region: Region): int = 
        (findNrOfCorners region) * (Set.count region)

    let printRegions (regions: Regions) =
        for region in regions do
            let ((_,ymax),c) = region |> Seq.maxBy (fun ((x,y),_) -> y) 
            let ((_,ymin),_) = region |> Seq.minBy (fun ((x,y),_) -> y)
            let ((xmax,_),_) = region |> Seq.maxBy (fun ((x,y),_) -> x)
            let ((xmin,_),_)  = region |> Seq.minBy (fun ((x,y),_) -> x)

            //printfn "Region %A from xmin %A to ymax %A" c xmin ymax
            printfn "Region %A ***"  c
            printfn "Cost is %A neighbords: %A area %A" (cost2 region) (findNrOfCorners region ) (Set.count region)
            for i in [xmin-2 .. xmax+2] do
                let mutable regionStr = ""
                for j in [ymin-2.. ymax+2] do
                    if Set.contains ((i,j),c) region then
                        let corners = countInRegionFor ((i,j),c) region
                        regionStr <- regionStr + (corners |> string) 
                    else
                        regionStr <- regionStr + "." 
                printfn "%A" regionStr
        ()

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
//        printfn "%A" input
        let regions = findRegions input
        let cost = regions |> List.map cost |> List.sum
        let cost2' = regions |> List.map cost2 |> List.sum
        printRegions regions 
        Assert.Equal(1930, cost)
        Assert.Equal(1206, cost2')
        let input = readInit "input2.txt" 
        let regions = findRegions input
        let cost2 = regions |> List.map cost2 |> List.sum
        Assert.Equal(830566, cost2) 

module Program = let [<EntryPoint>] main _ = 0