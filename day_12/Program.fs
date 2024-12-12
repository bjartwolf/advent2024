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

    let rec findRegions (input: char[][]) : Regions = 
        let rec findRegions' (input: char[][]) (x:int) (y:int) (region: Regions) : Regions = 
            []

        findRegions' input 0 0 []
    // areal og omkrets

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
//        printfn "%A" input
        let findRegions = findRegions input
        printfn "%A" findRegions
        Assert.Equal(10, input.Length) 

module Program = let [<EntryPoint>] main _ = 0