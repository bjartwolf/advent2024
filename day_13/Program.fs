open Xunit
open System

type Button = { X: int64; Y: int64}
type Price = { GX: int64; GY: int64}
type Machine = { A: Button; B: Button; P: Price }


let increase:int64 = 10000000000000L

module Input =
    open System
    open System.IO
    open Xunit 

    let parseButton (line: string): Button =
            let button1 = line.Split("+")
            let a = button1[1].Split(",")
            { X = (a.[0] |> int64); Y = button1.[2] |> int64 }

    let parsePrice (line: string): Price =
            let button1 = line.Split("=")
            let a = button1[1].Split(",")
            { GX = a.[0] |> int64; GY = button1.[2] |> int64 }

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
open Input

module Solver =
    type Presses = { PA: int64; PB: int64 }
    let checkPresses (machine: Machine) (presses: Presses): bool =
        (presses.PA*machine.A.X) + (presses.PB*machine.B.X) = machine.P.GX
        && (presses.PA*machine.A.Y) + (presses.PB*machine.B.Y) = machine.P.GY

    let costP (p: Presses) : int64 =
        3L*p.PA + p.PB

    let PaFromPb (machine: Machine)(Pb: int64): Presses option = 
        let Pad = ((double machine.P.GX-(double machine.P.GY*double machine.B.X/double machine.B.Y)))/(double machine.A.X - ((double machine.A.Y*double machine.B.X)/double machine.B.Y))
        let PadRounded = Math.Round(Pad, 5)
        let epsilon = 1e-5
        if Math.Abs(Pad - PadRounded) < epsilon then
            Some { PA = int64 PadRounded; PB = Pb }
        else
            None

    let solveMagic (machine: Machine) : (double*double) = 
        let a = ((double machine.P.GX-(double machine.P.GY*double machine.B.X/double machine.B.Y)))/(double machine.A.X - ((double machine.A.Y*double machine.B.X)/double machine.B.Y))
        let b = (double machine.P.GY) / (double machine.B.Y) - a * (double machine.A.Y) / (double machine.B.Y) 
        (a,b)

    let generatePresses (machine: Machine) (maxPress: int64): Presses seq =
        [
            let pressMachine = PaFromPb machine
            for j in 0L..100L do
                 let res = pressMachine j 
                 match res with
                    | None -> ()
                    | Some res-> res 
        ]
    let solve (machine: Machine) (maxPresses: int64) : (Presses*int64) option =
        let checkPressForMachine = checkPresses machine
        let solutions = generatePresses machine 100 |> Seq.filter(fun p -> checkPressForMachine p) 
        if (Seq.isEmpty solutions) then 
            None
        else
            let cheapest = 
                solutions 
                    |> Seq.map(fun p -> (p, costP p))
                    |> Seq.minBy (fun (_, c) -> c) 
            Some cheapest

    let solveMachines (machine: Machine list) : ((Presses*int64) option) seq =
        let maxPresses = increase 
        [ for m in machine do
            match solve m maxPresses with
            | Some (solution,cost) -> 
                printfn "*******"
                printfn "%A" m
                let magic = solveMagic m 
                printfn "magic %A" magic 
                printfn "solution %A" solution
                
                yield Some (solution, cost)
            | None -> 
                yield None ]

    let costOfAllMachines (machines: Machine list) : int64 =
        solveMachines machines
            |> Seq.choose(fun x -> x)
            |> Seq.sumBy(fun (_, c) -> c)

    let increaseXY (machine: Machine): Machine =
        { machine with  P = { GX = machine.P.GX + increase; GY = machine.P.GY + increase }}
open Solver

module Tests =
    open Input
    open Solver

    [<Fact>]
    let test2 () = 
        let machines = readInit "input1.txt" 
        let machine0 = machines.[0] 
        Assert.False(checkPresses machine0 { PA = 1; PB = 1 } )
        Assert.True(checkPresses machine0 { PA = 80; PB = 40 })
        Assert.True(checkPresses machines.[2] { PA = 38; PB = 86 })
        Assert.Equal(Some {PA = 80; PB = 40}, PaFromPb machine0 40)
        Assert.Equal(Some {PA = 38; PB = 86}, PaFromPb machines.[2] 86)
        Assert.Equal(280L, costP { PA = 80; PB = 40 } )
        let solutions = solveMachines machines |> Seq.toList
        Assert.Equivalent(Some ({ PA = 80; PB = 40 }, 280), solutions.[0])
        Assert.Equivalent(None, solutions.[1])
        Assert.Equivalent(Some ({ PA = 38; PB = 86 }, 200), solutions.[2])
        Assert.Equivalent(None, solutions.[3])
        Assert.Equivalent(480, costOfAllMachines machines)

    [<Fact>]
    let test3 () = 
        let machines = readInit "input2.txt" 
        Assert.Equivalent(36250, costOfAllMachines machines)

module Program = 
    let [<EntryPoint>] main _ = 
        let machines = readInit "input2.txt" 
        let expensiveMachines = machines //|> List.map increaseXY
        printfn "answer: %A " (costOfAllMachines expensiveMachines)
        Console.ReadLine() |> ignore
        0