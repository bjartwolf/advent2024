open Fable.Python.Builtins
open Day1Core

let file1 = builtins.``open``("..\input1.txt", "r").read()
let file2 = builtins.``open``("..\input2.txt", "r").read()

print(distance file1)
print(distance file2)
print(productOfInput file1)
print(productOfInput file2)
