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

    let readInit (filePath: string) =
        let lines = File.ReadAllLines(filePath) 
        lines.[lines.Length-1].Replace("Program: ","").Split(",") |> Array.map(fun x -> Int32.Parse(x)) |> Array.toList

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

    let eval (m:Machine) (p:Program) = 
        let opcode, literalOperand = p.[m.pc]
        let comboOperand = getComboOperand m literalOperand 
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

    let rec evalProgram (m: Machine) (p: Program): int list =
        match p |> List.tryItem m.pc with 
            | Some _ -> 
                let m' = eval m p
                evalProgram m' p 
            | None -> m.out

    [<Fact>]
    let testCodes2 () = 
        let input = readInit "input2.txt"  |> parseInputProgram
        let m = { A = 47006051; B = 0; C = 0; pc = 0 ; out = []}
        let result = evalProgram m input
        let commaSeparated = result |> List.rev |> List.map string |> String.concat ","
        Assert.Equal("6,2,7,2,3,1,6,0,5", commaSeparated) 

module Program = let [<EntryPoint>] main _ = 0