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

    let memoize f =
        let dict = Dictionary<_, _>();
        fun c ->
            let exist, value = dict.TryGetValue c
            match exist with
            | true -> value
            | _ -> 
                let value = f c
                dict.Add(c, value)
                value

                                                        
    let designMatchPatterns (patterns: string list) (design: string) : bool =
        let matchingPatternStripper = stripMatchingPatterns patterns |> memoize
        printfn "Checking %A" design
        let rec designMatchPatterns' (design: string) existCheck : bool =
            let strippedDesign = matchingPatternStripper design
            match strippedDesign with 
            | None -> false
            | Some matchedDesign ->  
                if matchedDesign |> List.contains ("") then 
                    true
                else existCheck matchedDesign 
        let exists = memoize (fun x -> List.exists (fun d -> designMatchPatterns' d ) x) 
        designMatchPatterns' design exists 
             
    let countPossibleDesigns (patterns: string list) (designs: string list) : int =
        designs 
            |> List.sortDescending
            |> List.filter (designMatchPatterns patterns) |> List.length


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
        ()
         

module Program = 
    open Advent
    let [<EntryPoint>] main _ = 
        let input2,designs2 = get_input "input2.txt"
        let count = countPossibleDesigns input2 designs2
        printfn "%A" count 
        System.Console.ReadKey() |> ignore
        0