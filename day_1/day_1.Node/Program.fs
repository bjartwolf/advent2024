open Node.Api
open Day1Core

let file1 = fs.readFileSync("..\input1.txt", "utf8")
let file2 = fs.readFileSync("..\input2.txt", "utf8")

printfn "%A" (Day1Core.distance file1)
printfn "%A" (Day1Core.distance file2)
printfn "%A" (Day1Core.productOfInput file1)
printfn "%A" (Day1Core.productOfInput file2)
