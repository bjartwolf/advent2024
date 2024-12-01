import * as fs from "fs";
import { productOfInput, distance } from "../day_1.Core/Program.fs.js";
import { printf, toConsole } from "./fable_modules/fable-library-js.4.24.0/String.js";

export const file1 = fs.readFileSync("..\\input1.txt", "utf8");

export const file2 = fs.readFileSync("..\\input2.txt", "utf8");

(function () {
    const arg = distance(file1) | 0;
    toConsole(printf("%A"))(arg);
})();

(function () {
    const arg = distance(file2) | 0;
    toConsole(printf("%A"))(arg);
})();

(function () {
    const arg = productOfInput(file1) | 0;
    toConsole(printf("%A"))(arg);
})();

(function () {
    const arg = productOfInput(file2) | 0;
    toConsole(printf("%A"))(arg);
})();

