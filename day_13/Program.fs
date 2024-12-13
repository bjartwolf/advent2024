module Input =
    open System
    open System.IO
    open Xunit 

    type Button = { X: int; Y: int}
    type Price = { GX: int; GY: int}

    type Machine = { A: Button; B: Button; P: Price }

    let parseButton (line: string): Button =
            let button1 = line.Split("+")
            let a = button1[1].Split(",")
            { X = (a.[0] |> int); Y = button1.[2] |> int }

    let parsePrice (line: string): Price =
            let button1 = line.Split("=")
            let a = button1[1].Split(",")
            { GX = a.[0] |> int; GY = button1.[2] |> int }

    [<Fact>]
    let parseButtonTest () = 
        let input = "Button A: X+94, Y+34"
        let button = parseButton input
        Assert.Equivalent({X = 94; Y= 34}, button)

    let readInit (filePath: string): Machine list = 
        let file = File.ReadAllText(filePath)
        let machines = file.Split(sprintf "%s%s" Environment.NewLine  Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
        [
        for line in machines do
            let lines = line.Split(sprintf "%s" Environment.NewLine, StringSplitOptions.RemoveEmptyEntries)
            let a = parseButton lines.[0]
            let b = parseButton lines.[1]
            let c = parsePrice lines.[2]
            yield { A = a; B=b; P=c } 
        ] 


    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(4, input.Length) 
        Assert.Equal({ A = { X = 94; Y=34}; B = { X = 22; Y = 67}; P = { GX = 8400; GY = 5400 }} , input.Head) 

module Program = let [<EntryPoint>] main _ = 0