open Browser
open Browser.Dom
open Fable.Core.JsInterop
open Browser.Types
open Day1Core
let div = document.createElement "div"
document.body.appendChild div |> ignore
let canvas1 = document.createElement "canvas" :?> HTMLCanvasElement
canvas1.width <- 1000
canvas1.height <-500 
let canvas2 = document.createElement "canvas" :?> HTMLCanvasElement
canvas2.width <- 1000
canvas2.height <-500 

document.body.appendChild canvas1 |> ignore
document.body.appendChild canvas2 |> ignore
let ctx1 = canvas1.getContext_2d()
let ctx2 = canvas2.getContext_2d()
let clear (canvas:CanvasRenderingContext2D) =
    canvas.clearRect(0, 0, canvas.canvas.width, canvas.canvas.height)
let drawLines (canvas:CanvasRenderingContext2D) (lst1: int list) (color:string) (scale: int)=
    canvas.clearRect(0, 0, canvas.canvas.width, canvas.canvas.height)
    for (i,val1) in (List.mapi (fun i num1 -> (i, num1)) lst1 ) do
    canvas.strokeStyle <- !^color
    canvas.beginPath()
    canvas.moveTo(0, i)
    canvas.lineTo(int(val1/scale),i)
    canvas.closePath()
    canvas.stroke()


let mutable counter = 0
let updateDiv () =
    if (0 <= counter && counter <= 2 ) then
        div.innerHTML <- sprintf "Raw input"
        let (lst1, lst2) = Input.input |> parseInput |> split
        drawLines ctx1 lst1 "red" 100
        drawLines ctx2 lst2 "blue" 100
    if (2 < counter && counter <= 3 ) then
        div.innerHTML <- sprintf "Sorted input"
        let (lst1, lst2) = Input.input |> parseInput |> splitAndSort
        drawLines ctx1 lst1 "red" 100
        drawLines ctx2 lst2 "blue" 100
    if (counter > 5 && counter <=6) then
        div.innerHTML <- sprintf "The diff is the chart and result sum of the diff is %i" (Input.input |> distance)
        let lst1 = Input.input |> parseInput |> splitAndSort |> findDiff 
        drawLines ctx1 lst1 "red" 100
        clear ctx2
    if (counter > 6 && counter <=8) then
        div.innerHTML <- sprintf "The occurances are plot below and the product are %i" (Input.input |> productOfInput)
        let lst = Input.input |> parseInput |> split |> countOccurrances 
        let lst1, lst2 = List.unzip lst
        drawLines ctx1 lst1 "red" 100
        drawLines ctx2 lst2 "blue" 1
    counter <- counter + 1

let intervalId = window.setInterval(updateDiv, 1000)

let stopAfter10Iterations () =
    if counter >= 10 then
        window.clearInterval(intervalId)

window.setInterval(stopAfter10Iterations, 1000) |> ignore
