import { createAtom, disposeSafe, getEnumerator } from "./fable_modules/fable-library-js.4.24.0/Util.js";
import { unzip, mapIndexed } from "./fable_modules/fable-library-js.4.24.0/List.js";
import { printf, toText } from "./fable_modules/fable-library-js.4.24.0/String.js";
import { countOccurrances, productOfInput, findDiff, distance, splitAndSort, parseInput, split } from "../day_1.Core/Program.fs.js";
import { input as input_6 } from "./input.fs.js";

export const div = document.createElement("div");

document.body.appendChild(div);

export const canvas1 = document.createElement("canvas");

canvas1.width = 1000;

canvas1.height = 500;

export const canvas2 = document.createElement("canvas");

canvas2.width = 1000;

canvas2.height = 500;

document.body.appendChild(canvas1);

document.body.appendChild(canvas2);

export const ctx1 = canvas1.getContext('2d');

export const ctx2 = canvas2.getContext('2d');

export function clear(canvas) {
    canvas.clearRect(0, 0, canvas.canvas.width, canvas.canvas.height);
}

export function drawLines(canvas, lst1, color, scale) {
    canvas.clearRect(0, 0, canvas.canvas.width, canvas.canvas.height);
    const enumerator = getEnumerator(mapIndexed((i, num1) => [i, num1], lst1));
    try {
        while (enumerator["System.Collections.IEnumerator.MoveNext"]()) {
            const forLoopVar = enumerator["System.Collections.Generic.IEnumerator`1.get_Current"]();
            const i_1 = forLoopVar[0] | 0;
            canvas.strokeStyle = color;
            canvas.beginPath();
            canvas.moveTo(0, i_1);
            canvas.lineTo(~~(forLoopVar[1] / scale), i_1);
            canvas.closePath();
            canvas.stroke();
        }
    }
    finally {
        disposeSafe(enumerator);
    }
}

export let counter = createAtom(0);

export function updateDiv() {
    let arg, tupledArg, arg_1, tupledArg_1;
    if ((0 <= counter()) && (counter() <= 2)) {
        div.innerHTML = toText(printf("Raw input"));
        const patternInput = split(parseInput(input_6));
        drawLines(ctx1, patternInput[0], "red", 100);
        drawLines(ctx2, patternInput[1], "blue", 100);
    }
    if ((2 < counter()) && (counter() <= 3)) {
        div.innerHTML = toText(printf("Sorted input"));
        const patternInput_1 = splitAndSort(parseInput(input_6));
        drawLines(ctx1, patternInput_1[0], "red", 100);
        drawLines(ctx2, patternInput_1[1], "blue", 100);
    }
    if ((counter() > 5) && (counter() <= 6)) {
        div.innerHTML = ((arg = (distance(input_6) | 0), toText(printf("The diff is the chart and result sum of the diff is %i"))(arg)));
        drawLines(ctx1, (tupledArg = splitAndSort(parseInput(input_6)), findDiff(tupledArg[0], tupledArg[1])), "red", 100);
        clear(ctx2);
    }
    if ((counter() > 6) && (counter() <= 8)) {
        div.innerHTML = ((arg_1 = (productOfInput(input_6) | 0), toText(printf("The occurances are plot below and the product are %i"))(arg_1)));
        const patternInput_2 = unzip((tupledArg_1 = split(parseInput(input_6)), countOccurrances(tupledArg_1[0], tupledArg_1[1])));
        drawLines(ctx1, patternInput_2[0], "red", 100);
        drawLines(ctx2, patternInput_2[1], "blue", 1);
    }
    counter(counter() + 1);
}

export const intervalId = window.setInterval(() => {
    updateDiv();
}, 1000);

export function stopAfter10Iterations() {
    if (counter() >= 10) {
        window.clearInterval(intervalId);
    }
}

window.setInterval(() => {
    stopAfter10Iterations();
}, 1000);

