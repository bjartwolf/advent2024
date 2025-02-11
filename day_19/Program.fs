open System
open System.Collections.Generic

module Advent =
    open System.IO
    open Xunit 
    let get_input (filename: string) : string list*string list= 
        let lines = File.ReadAllLines(filename)
        let patterns = lines.[0].Split(",",StringSplitOptions.TrimEntries) |> Array.toList
        patterns,lines |> Array.toList |> List.skip 2

    let stripMatchingPatterns (patterns: string list) (design: string) : string list option = 
        let foo = patterns |> List.map (fun pattern -> let patternMatch = design.StartsWith(pattern)
                                                       if patternMatch then
                                                              let lengthOfMatch = pattern.Length
                                                              Some (design.Substring(lengthOfMatch))
                                                          else
                                                              None ) 
        let hits = foo |> List.choose id
        if hits = [] then None
        else Some hits

    let designMatchPatterns (patterns: string list) (design: string) : bool =
        let mutable nomatch = Set.empty 
        let matchingPatternStripper = stripMatchingPatterns patterns 
        let rec designMatchPatterns' (design: string) : bool =
            let strippedDesign = matchingPatternStripper design
            match strippedDesign with 
            | None -> false
            | Some matchedDesign ->  
                if matchedDesign |> List.contains ("") then 
                    true
                else 
                    match matchedDesign with 
                        | [] -> false
                        | h :: t ->  
                                if nomatch.Contains(h) then
                                    false
                                else 
                                    let result = designMatchPatterns' h
                                    if not result then 
                                        nomatch<- nomatch.Add(h) 
                                        t |> List.exists (fun d -> designMatchPatterns' d) 
                                    else 
                                        result 
        designMatchPatterns' design

    let designMatchPatternsCount (patterns: string list) (design: string) : int64 =
        let mutable variations: Map<string,int64> = Map.empty 
        let matchingPatternStripper = stripMatchingPatterns patterns 
        let rec designMatchPatterns' (design: string) : int64 =
            let strippedDesign = matchingPatternStripper design
            match strippedDesign with 
            | None -> 0L
            | Some matchedDesign -> let foundMatches = (matchedDesign |> List.filter (fun x -> x = "") |> List.length) |> int64
                                    let newMatches = 
                                        match matchedDesign |> List.filter (fun x -> x <> "") with 
                                            | [] -> 0L
                                            | h ->  let allKeys = variations |> Map.keys |> Seq.toList
                                                    let knownKeys = h |> List.filter (fun k -> List.contains k allKeys)
                                                    let newKeys = List.except knownKeys h
                                                    let knownValues = Map.filter (fun k _ -> knownKeys |> Seq.contains k) variations |> Map.toSeq |> Seq.sumBy (fun (_,v) -> v)
                                                    let newValues = newKeys |> Seq.sumBy (fun k -> 
                                                        let result = designMatchPatterns' k
                                                        variations <- variations.Add(k,result)
                                                        result)
                                                    newValues + knownValues 
                                    foundMatches + newMatches
        designMatchPatterns' design
              
    let countPossibleDesigns (patterns: string list) (designs: string list) : int =
        designs 
            |> List.sortDescending
            |> List.filter (designMatchPatterns patterns ) |> List.length

    let countAllPossibleDesigns (patterns: string list) (designs: string list) : int64 =
        designs 
            |> List.map (fun x -> designMatchPatternsCount patterns x) 
            |> List.sum


    [<Fact>]
    let test2 () = 
        let input1,designs1 = get_input "input1.txt"
        let stripMatchingPatterns1 = stripMatchingPatterns input1
        Assert.Equivalent(Some [""], stripMatchingPatterns1 "wr" )
        Assert.Equivalent(Some ["b" ], stripMatchingPatterns1 "wrb" )
        Assert.Equivalent(Some ["bu"; "u" ], stripMatchingPatterns1 "rbu" )
        Assert.Equivalent(Some ["bux"; "ux" ], stripMatchingPatterns1 "rbux" )
        Assert.Equal(1, countPossibleDesigns ["r"] ["r"])
        Assert.Equal(0, countPossibleDesigns ["r"] ["rb"])
        Assert.Equal(0, countPossibleDesigns ["rb"] ["r"])
        Assert.Equal(1, countPossibleDesigns ["r"] ["rb";"r"])
        Assert.True(designMatchPatterns input1 "brwrr")
        Assert.True(designMatchPatterns input1 "bggr")
        Assert.True(designMatchPatterns input1 "gbbr")
        Assert.True(designMatchPatterns input1 "rrbgbr")
        Assert.False(designMatchPatterns input1 "ubwu")
        Assert.True(designMatchPatterns input1 "bwurrg")
        Assert.True(designMatchPatterns input1 "brgr")
        Assert.False(designMatchPatterns input1 "bbrgwb")
        Assert.Equal(6, countPossibleDesigns input1 designs1)
        Assert.Equal(16L, countAllPossibleDesigns input1 designs1)
        let input2,designs2 = get_input "input2.txt"
        Assert.Equal(311, countPossibleDesigns input2 designs2)
        ()
         

module Program = 
    open Advent
    let [<EntryPoint>] main _ = 
        let input2,designs2 = get_input "input2.txt"
        //let count = countPossibleDesigns input2 designs2
        //printfn "%A" count 
        let countAll = countAllPossibleDesigns input2 designs2
        printfn "%A" countAll
        System.Console.ReadKey() |> ignore
        0