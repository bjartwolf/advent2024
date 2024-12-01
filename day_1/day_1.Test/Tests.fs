module Tests

open Xunit
open Day1Core

let readInit (filePath: string): string = 
    System.IO.File.ReadAllText(filePath) 

[<Fact>]
let test () = 
    let input = readInit "input1.txt"  
    Assert.Equal(11, distance input)
    Assert.Equal(31, productOfInput input) 

[<Fact>]
let test2 () = 
    let input = readInit "input2.txt" 
    Assert.Equal(18934359, productOfInput input)


