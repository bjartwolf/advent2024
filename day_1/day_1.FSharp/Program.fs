module Input

let readInit (filePath: string): string = 
    System.IO.File.ReadAllText(filePath) 

module Program = 
    open Day1Core
    let [<EntryPoint>] main _ = 
        let input1 = readInit "input1.txt" 
        let input2 = readInit "input2.txt" 
        printfn "%A" (distance input1)
        printfn "%A" (distance input2)
        printfn "%A" (productOfInput input1)
        printfn "%A" (productOfInput input2)
        0



