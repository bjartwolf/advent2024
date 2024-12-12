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

    let findCandidates (pos: Position): Position list =
        let (x,y) = pos 
        let up = (x,y-1) 
        let left = (x-1,y) 
        let down = (x,y+1) 
        let right = (x+1,y) 
        let candidates = [up;left;down;right]
        candidates 

    let findNeighbors (region: Region): Position Set =
        region |> Set.map (fun (p,_) -> findCandidates p |> Set.ofList) |> Set.unionMany

    let findNeighborCount (region: Region): int =
        let positionsInSet = region |> Set.map (fun (p,t) -> p)
        let neighbors = findNeighbors region
        Set.difference neighbors positionsInSet |> Set.count

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
                            let cs = candidates |> List.map (fun c -> regionIndex c) |> List.choose id
                            match cs with
                                | []-> findRegions' restPos (Set.singleton (pos, tileType) :: regions)
                                | i::j:: _ -> // ouch, two regions. must merge then, remove the other from the list
                                              let newRegion =  regions.[i] |> Set.add (pos,tileType)
                                              let mergedRegions = Set.union regions.[i] regions.[j] 
                                              // delete region j
                                              let newRegions = regions |> List.updateAt i mergedRegions 
                                              let removedRegions = newRegions |> List.removeAt j 
                                              findRegions' restPos removedRegions 
                                | i:: _ -> let newRegion =  regions.[i] |> Set.add (pos,tileType)
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
    let cost (region: Region): int = 
        (findNeighbors region |> Set.count) * (Set.count region)

    let printRegions (regions: Regions) =
        for region in regions do
            let ((_,ymax),c) = region |> Seq.maxBy (fun ((x,y),_) -> y)
            let ((_,ymin),_) = region |> Seq.minBy (fun ((x,y),_) -> y)
            let ((xmax,_),_) = region |> Seq.maxBy (fun ((x,y),_) -> x)
            let ((xmin,_),_)  = region |> Seq.minBy (fun ((x,y),_) -> x)

            //printfn "Region %A from xmin %A to ymax %A" c xmin ymax
            printfn "Region %A ***"  c
            printfn "Cost is %A neighbords: %A circ %A" (cost region) (findNeighbors region |> Set.count) (Set.count region)
            for i in [xmin .. xmax] do
                for j in [ymin.. ymax] do
                    if Set.contains ((i,j),c) region then
                        printf("%A") (c |> string)
                    else
                        printf(".")
                printfn ""
        ()

    [<Fact>]
    let test2 () = 
        let input = readInit "input0.txt" 
//        printfn "%A" input
        let regions = findRegions input
        printRegions regions 
//        Assert.Equal(10, input.Length) 

module Program = let [<EntryPoint>] main _ = 0