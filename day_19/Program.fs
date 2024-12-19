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

    let designMatchPatterns (patterns: string list) (nomatch: Set<string> ref) (design: string) : bool =
        let matchingPatternStripper = stripMatchingPatterns patterns 
        printfn "Checking %A" design
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
                                if nomatch.Value.Contains(h) then
                                    false
                                else 
                                    let result = designMatchPatterns' h
                                    printfn "checking %A is %A" h result
                                    if not result then 
                                        nomatch.Value <- (nomatch.Value).Add(h) 
                                        t |> List.exists (fun d -> designMatchPatterns' d) 
                                    else 
                                        result 
        designMatchPatterns' design
             
    let countPossibleDesigns (patterns: string list) (designs: string list) : int =
        let nomatch = ref Set.empty 
        designs 
            |> List.sortDescending
            |> List.filter (designMatchPatterns patterns nomatch ) |> List.length


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
        Assert.True(designMatchPatterns input1 (ref Set.empty) "brwrr")
        Assert.True(designMatchPatterns input1 (ref Set.empty) "bggr")
        Assert.True(designMatchPatterns input1 (ref Set.empty) "gbbr")
        Assert.True(designMatchPatterns input1 (ref Set.empty) "rrbgbr")
        Assert.False(designMatchPatterns input1 (ref Set.empty) "ubwu")
        Assert.True(designMatchPatterns input1 (ref Set.empty) "bwurrg")
        Assert.True(designMatchPatterns input1 (ref Set.empty) "brgr")
        Assert.False(designMatchPatterns input1 (ref Set.empty) "bbrgwb")
        Assert.Equal(6, countPossibleDesigns input1 designs1)
        let input2,designs2 = get_input "input2.txt"
        Assert.NotEqual(307, countPossibleDesigns input2 designs2)
        ()
         

module Program = 
    open Advent
    let [<EntryPoint>] main _ = 
        let input2,designs2 = get_input "input2.txt"
        let count = countPossibleDesigns input2 designs2
        printfn "%A" count 
        System.Console.ReadKey() |> ignore
        0