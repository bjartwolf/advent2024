import { split } from "../day_1.Node/fable_modules/fable-library-js.4.24.0/String.js";
import { fold, zip, sumBy, map as map_1, sort, ofArray } from "../day_1.Node/fable_modules/fable-library-js.4.24.0/List.js";
import { item, map } from "../day_1.Node/fable_modules/fable-library-js.4.24.0/Array.js";
import { parse } from "../day_1.Node/fable_modules/fable-library-js.4.24.0/Int32.js";
import { numberHash, comparePrimitives } from "../day_1.Node/fable_modules/fable-library-js.4.24.0/Util.js";
import { tryFind, ofList } from "../day_1.Node/fable_modules/fable-library-js.4.24.0/Map.js";
import { List_countBy } from "../day_1.Node/fable_modules/fable-library-js.4.24.0/Seq2.js";
import { defaultArg } from "../day_1.Node/fable_modules/fable-library-js.4.24.0/Option.js";

export function splitOnNewlines(input) {
    return split(input, ["\r", "\n"], undefined, 1);
}

export function parseInput(input) {
    let array_1;
    return ofArray(map((strings) => [parse(item(0, strings), 511, false, 32), parse(item(1, strings), 511, false, 32)], (array_1 = map((line) => split(line, ["  "], undefined, 0), splitOnNewlines(input)), array_1.filter((line_1) => (line_1.length > 0)))));
}

export function splitAndSortList(pairList) {
    return [sort(map_1((tuple) => tuple[0], pairList), {
        Compare: comparePrimitives,
    }), sort(map_1((tuple_1) => tuple_1[1], pairList), {
        Compare: comparePrimitives,
    })];
}

export function findDistance(l1, l2) {
    return sumBy((tupledArg) => {
        const n1 = tupledArg[0] | 0;
        const n2 = tupledArg[1] | 0;
        return Math.abs(n1 - n2) | 0;
    }, zip(l1, l2), {
        GetZero: () => 0,
        Add: (x, y) => (x + y),
    });
}

export function countOccurrances(l1, l2) {
    const counts = ofList(List_countBy((x) => x, l2, {
        Equals: (x_1, y) => (x_1 === y),
        GetHashCode: numberHash,
    }), {
        Compare: comparePrimitives,
    });
    return map_1((num1) => [num1, defaultArg(tryFind(num1, counts), 0)], l1);
}

export function productOfOccurances(counts) {
    return fold((acc, tupledArg) => {
        const num = tupledArg[0] | 0;
        const count = tupledArg[1] | 0;
        return (acc + (num * count)) | 0;
    }, 0, counts);
}

export function distance(input) {
    const tupledArg = splitAndSortList(parseInput(input));
    return findDistance(tupledArg[0], tupledArg[1]) | 0;
}

export function productOfInput(input) {
    let tupledArg;
    return productOfOccurances((tupledArg = splitAndSortList(parseInput(input)), countOccurrances(tupledArg[0], tupledArg[1])));
}

