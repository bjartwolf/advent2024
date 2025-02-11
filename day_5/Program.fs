module Input =
    open System
    open System.IO
    open Xunit 


    let parseRule (rule: string): int*int =
        let splitRule = rule.Split("|", StringSplitOptions.RemoveEmptyEntries)
        (int splitRule.[0], int splitRule.[1])

    let readInit (filePath: string): (int*int) list * (int list) list = 
        let txt = File.ReadAllText filePath 
        let splitTxt = txt.Split(System.Environment.NewLine + System.Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
        let rules = splitTxt.[0].Split(System.Environment.NewLine, StringSplitOptions.RemoveEmptyEntries) |> Array.map parseRule |> Array.toList
        let lists = splitTxt.[1].Split(System.Environment.NewLine, StringSplitOptions.RemoveEmptyEntries) 
                                        |> Array.map (fun x -> x.Split(",", StringSplitOptions.RemoveEmptyEntries))
                                        |> Array.map (fun x -> x |> Array.map int |> Array.toList) |> Array.toList 
        (rules , lists) 

    let numIsInOrder (rules: (int*int) list)  (num: int) (numbers: int list) : bool = 
        numbers |> List.forall (fun numberToCheck -> rules |> List.exists (fun (ruleX,ruleY) -> ruleX = num && ruleY = numberToCheck))


    let rec isSorted (rules: (int*int) list) (list: int list) = 
        match list with
            | h :: t -> numIsInOrder rules h t && isSorted rules t
            | _ -> true

    let compare (rules: (int*int) list) (a: int) (b: int) = 
        let rule1 = rules |> List.exists (fun (ruleX,ruleY) -> ruleX = a && ruleY = b)
        if rule1 then 1 
        else -1

    let sortWithRules rules (lst: int list) = 
        lst |> List.sortWith (fun a b -> compare rules a b)

    let middle (lst: int list): int = 
        lst.[lst.Length/2]

    let solve1 (rules: (int*int) list) (lists: (int list) list) = 
        lists |> List.filter (fun x -> isSorted rules x) |> List.sumBy middle 

    let solve2 (rules: (int*int) list) (lists: (int list) list) = 
        lists |> List.filter (fun x -> not (isSorted rules x)) |> List.map (fun x -> sortWithRules rules x) |>List.sumBy middle 


    [<Fact>]
    let test2 () = 
        let (rules, lists ) = readInit "input1.txt" 
        Assert.Equal(21, rules.Length) 
        Assert.Equal(61, middle lists.[0]) 
        Assert.True(numIsInOrder rules (List.head lists.[0])  (List.tail lists.[0]))
        Assert.True(isSorted rules lists.[0]) 
        Assert.True(isSorted rules lists.[1]) 
        Assert.True(isSorted rules lists.[2]) 
        Assert.False(isSorted rules lists.[3]) 
        Assert.False(isSorted rules lists.[4]) 
        Assert.False(isSorted rules lists.[4]) 
        Assert.Equal(143 , solve1 rules lists)
        Assert.Equal(123 , solve2 rules lists)
        let (rules, lists ) = readInit "input2.txt" 
        Assert.Equal(4462 , solve1 rules lists)
        Assert.Equal(6767, solve2 rules lists)

module Program = let [<EntryPoint>] main _ = 0