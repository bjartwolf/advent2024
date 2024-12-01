open Browser
let div = document.createElement "div"

document.body.appendChild div |> ignore
let res = Day1Core.productOfInput Input.input
div.innerHTML <- sprintf "The result is %i" res
