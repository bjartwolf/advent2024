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
        []

    [<Fact>]
    let test2 () = 
        let input = readInit "input1.txt" 
        Assert.Equal(1, 1) 


module Machine = 
    let getComboOperand (m: Machine) (operand: int): int = 
        match operand with 
            | num when num = 1 || num = 2 || num = 3 -> num
            | 4 -> m.A
            | 5 -> m.B
            | 6 -> m.C
            | _ -> failwith "Invalid operand"

    let eval (m: Machine) (p: Program ): Machine =
        let opcode, literalOperand = p.[m.pc]
        let comboOperand = getComboOperand m literalOperand 
        match opcode  with 
            | Adv -> { m with A = m.A / (pown 2 comboOperand ); pc = m.pc+2; }
            | Bxl -> { m with B = m.B ^^^ literalOperand ; pc = m.pc+2; }
            | Bst -> { m with B = comboOperand % 8; pc = m.pc+2}
            | Jnz -> if m.A = 0 then { m with out = []; pc = m.pc+2 }
                     else { m with pc = literalOperand ; }
            | Bxc -> { m with B = m.B ^^^ m.C ; pc = m.pc+2 }
            | Out -> { m with out = (comboOperand % 8) :: m.out ; pc = m.pc+2 }
            | Bdv -> { m with B = m.A / (pown 2 comboOperand ); pc = m.pc+2; out = [] }
            | Cdv -> { m with C = m.A / (pown 2 comboOperand ); pc = m.pc+2; out = [] }

    [<Fact>]
    let testCodes () = 
        let p = [(Adv, 2)]
        let m = { A = 16; B = 0; C = 0; pc = 0 ; out = []}
        Assert.Equal(4, (eval m p).A)

        let p2 = [(Adv, 5)]
        let m2 = { A = 64; B = 3; C = 0; pc = 0; out = [] }
        Assert.Equal(8, (eval m2 p2).A)

module Program = let [<EntryPoint>] main _ = 0