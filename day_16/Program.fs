open Xunit

type Machine = { A: int
                 B: int;
                 C: int;
                 pc: int;
                 out: int list}
                 
type ProgramCode = int list
type OpCode = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv
type Program = (OpCode*int) list


module Input =
    open System
    open System.IO
    open Xunit 

    let readInit (filePath: string): int list = 
        let lines = File.ReadAllLines(filePath) 
        lines.[lines.Length-1].Replace("Program: ","").Split(",") |> Array.map(fun x -> Int32.Parse(x)) |> Array.toList


    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equivalent([0;1;5;4;3;0], input ) 

    let parseInputProgram (input: int list): Program = 
        let rec parseInputProgram' (input: int list): Program = 
            match input with 
                | [] -> [] 
                | x::y::xs -> 
                    match x with 
                        | 0 -> (Adv, y):: parseInputProgram' xs 
                        | 1 -> (Bxl, y):: parseInputProgram' xs 
                        | 2 -> (Bst, y):: parseInputProgram' xs 
                        | 3 -> (Jnz, y):: parseInputProgram' xs 
                        | 4 -> (Bxc, y):: parseInputProgram' xs 
                        | 5 -> (Out, y):: parseInputProgram' xs 
                        | 6 -> (Bdv, y):: parseInputProgram' xs 
                        | 7 -> (Cdv, y):: parseInputProgram' xs 
                        | _ -> failwith "Invalid opcode"
                | _ -> failwith "Invalid input"
        parseInputProgram' input 

    [<Fact>]
    let testinputParsing () = 
// Program: 0,1,5,4,3,0 
        let input = readInit "input1.txt"  |> parseInputProgram
        Assert.Equivalent([(Adv, 1);(Out, 4);(Jnz,0)], input ) 


module Machine = 
    open Input
    let getComboOperand (m: Machine) (operand: int): int = 
        match operand with 
            | num when num = 0 || num = 1 || num = 2 || num = 3 -> num
            | 4 -> m.A
            | 5 -> m.B
            | 6 -> m.C
            | _ -> failwith "Invalid operand"

    let eval (m: Machine) (p: Program ): Machine =
        match p |> List.tryItem m.pc with 
        | Some(opcode, literalOperand) ->
            let comboOperand = getComboOperand m literalOperand 
//            printfn "%A" m
            match opcode  with 
                | Adv -> { m with A = m.A / (pown 2 comboOperand ); pc = m.pc+1; }
                | Bxl -> { m with B = m.B ^^^ literalOperand ; pc = m.pc+1; }
                | Bst -> { m with B = comboOperand % 8; pc = m.pc+1}
                | Jnz -> if m.A = 0 then { m with pc = m.pc+1 }
                         else { m with pc = literalOperand ; }
                | Bxc -> { m with B = m.B ^^^ m.C ; pc = m.pc+1 }
                | Out -> { m with out = (comboOperand % 8) :: m.out ; pc = m.pc+1 }
                | Bdv -> { m with B = m.A / (pown 2 comboOperand ); pc = m.pc+1; }
                | Cdv -> { m with C = m.A / (pown 2 comboOperand ); pc = m.pc+1; }
                | _ -> m 
        | None -> m 

    let rec evalProgram (m: Machine) (p: Program): int list =
        match p |> List.tryItem m.pc with 
            | Some inst -> 
                let m' = eval m p
                evalProgram m' p 
            | None -> m.out

    [<Fact>]
    let testInput () = 
        let input = [2;6] |> parseInputProgram
        let m = { A = 0; B = 0; C = 9; pc = 0 ; out = []}
        let result = eval m input 
        Assert.Equivalent(1, result.B)

    [<Fact>]
    let testInput2 () = 
        let input = [5;0;5;1;5;4] |> parseInputProgram
        let m = { A = 10; B = 0; C = 0; pc = 0 ; out = []}
        let result = evalProgram m input 
        Assert.Equivalent([0;1;2], result)

    [<Fact>]
    let testInput3 () = 
        let input = [0;1;5;4;3;0] |> parseInputProgram
        printfn "Program %A" input
        let m = { A = 2024; B = 0; C = 0; pc = 0 ; out = []}
        let result = evalProgram m input 
        printfn "%A" result
        Assert.Equivalent([4;2;5;6;7;7;7;7;3;1;0], result)



    [<Fact>]
    let testCodes1 () = 
        let input = readInit "input1.txt"  |> parseInputProgram
        let m = { A = 729; B = 0; C = 0; pc = 0 ; out = []}
        let result = evalProgram m input
        Assert.Equivalent([4;6;3;5;6;3;5;2;1;0], result)
        let commaSeparated = result |> List.rev |> List.map string |> String.concat ","
        Assert.Equal("4,6,3,5,6,3,5,2,1,0", commaSeparated) 

    [<Fact>]
    let testCodes2 () = 
        let input = readInit "input2.txt"  |> parseInputProgram
        let m = { A = 47006051; B = 0; C = 0; pc = 0 ; out = []}
        let result = evalProgram m input
        let commaSeparated = result |> List.rev |> List.map string |> String.concat ","
        Assert.Equal("4,6,3,5,6,3,5,2,1,0", commaSeparated) 


    [<Fact>]
    let testCodes () = 
        let p = [(Adv, 2)]
        let m = { A = 16; B = 0; C = 0; pc = 0 ; out = []}
        Assert.Equal(4, (eval m p).A)

        let p2 = [(Adv, 5)]
        let m2 = { A = 64; B = 3; C = 0; pc = 0; out = [] }
        Assert.Equal(8, (eval m2 p2).A)

        let p3 = [(Bst, 6)]
        let m3 = { A = 0; B = 0; C = 9; pc = 0; out = [] }
        Assert.Equal(1, (eval m3 p3).B)

        let p4 = [(Bst, 6)]
        let m4 = { A = 0; B = 0; C = 9; pc = 0; out = [] }
        Assert.Equal(1, (eval m4 p4).B)


module Program = let [<EntryPoint>] main _ = 0