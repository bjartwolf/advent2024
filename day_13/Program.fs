open Xunit

type Button = { X: int; Y: int}
type Price = { GX: int; GY: int}
type Machine = { A: Button; B: Button; P: Price }


module Input =
    open System
    open System.IO
    open Xunit 

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

module Solver =
    type Presses = { PA: int; PB: int }
    let checkPresses (machine: Machine) (presses: Presses): bool =
        (presses.PA*machine.A.X) + (presses.PB*machine.B.X) = machine.P.GX
        && (presses.PA*machine.A.Y) + (presses.PB*machine.B.Y) = machine.P.GY

    let costP (p: Presses) : int =
        3*p.PA + p.PB

    let generatePresses (maxPress: int): Presses seq =
        [
            for i in 0..maxPress do
                for j in 0..maxPress do
                    yield { PA = i; PB = j }
        ]
    let solve (machine: Machine) (maxPresses: int) : (Presses*int) option =
        let checkPressForMachine = checkPresses machine
        let solutions = generatePresses 100 |> Seq.filter(fun p -> checkPressForMachine p) 
        if (Seq.isEmpty solutions) then 
            None
        else
            let cheapest = 
                solutions 
                    |> Seq.map(fun p -> (p, costP p))
                    |> Seq.minBy (fun (_, c) -> c) 
            Some cheapest

    let solveMachines (machine: Machine list) : ((Presses*int) option) seq =
        let maxPresses = 100
        [ for m in machine do
            match solve m maxPresses with
            | Some x -> yield Some x
            | None -> yield None ]

    let costOfAllMachines (machines: Machine list) : int =
        solveMachines machines
            |> Seq.choose(fun x -> x)
            |> Seq.sumBy(fun (_, c) -> c)

module Tests =
    open Input
    open Solver

    [<Fact>]
    let test2 () = 
        let machines = readInit "input1.txt" 
        let machine0 = machines.[0] 
        Assert.False(checkPresses machine0 { PA = 1; PB = 1 } )
        Assert.True(checkPresses machine0 { PA = 80; PB = 40 })
        Assert.Equal(280, costP { PA = 80; PB = 40 } )
        let solutions = solveMachines machines |> Seq.toList
        Assert.Equivalent(Some ({ PA = 80; PB = 40 }, 280), solutions.[0])
        Assert.Equivalent(None, solutions.[1])
        Assert.Equivalent(Some ({ PA = 38; PB = 86 }, 200), solutions.[2])
        Assert.Equivalent(None, solutions.[3])
        Assert.Equivalent(480, costOfAllMachines machines)

    [<Fact>]
    let test3 () = 
        let machines = readInit "input1.txt" 
        Assert.Equivalent(480, costOfAllMachines machines)


module Program = let [<EntryPoint>] main _ = 0