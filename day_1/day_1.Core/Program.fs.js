import { split as split_1 } from "../day_1.Browser/fable_modules/fable-library-js.4.24.0/String.js";
import { fold, zip, sumBy, sort, map as map_1, ofArray } from "../day_1.Browser/fable_modules/fable-library-js.4.24.0/List.js";
import { item, map } from "../day_1.Browser/fable_modules/fable-library-js.4.24.0/Array.js";
import { parse } from "../day_1.Browser/fable_modules/fable-library-js.4.24.0/Int32.js";
import { numberHash, comparePrimitives } from "../day_1.Browser/fable_modules/fable-library-js.4.24.0/Util.js";
import { tryFind, ofList } from "../day_1.Browser/fable_modules/fable-library-js.4.24.0/Map.js";
import { List_countBy } from "../day_1.Browser/fable_modules/fable-library-js.4.24.0/Seq2.js";
import { defaultArg } from "../day_1.Browser/fable_modules/fable-library-js.4.24.0/Option.js";

export function splitOnNewlines(input) {
    return split_1(input, ["\r", "\n"], undefined, 1);
}

export function parseInput(input) {
    let array_1;
    return ofArray(map((strings) => [parse(item(0, strings), 511, false, 32), parse(item(1, strings), 511, false, 32)], (array_1 = map((line) => split_1(line, ["  "], undefined, 0), splitOnNewlines(input)), array_1.filter((line_1) => (line_1.length > 0)))));
}

export function split(pairList) {
    return [map_1((tuple) => tuple[0], pairList), map_1((tuple_1) => tuple_1[1], pairList)];
}

export function splitAndSort(pairList) {
    return [sort(map_1((tuple) => tuple[0], pairList), {
        Compare: comparePrimitives,
    }), sort(map_1((tuple_1) => tuple_1[1], pairList), {
        Compare: comparePrimitives,
    })];
}

export function findDistance(l1, l2) {
    return sumBy((tupledArg) => Math.abs(tupledArg[0] - tupledArg[1]), zip(l1, l2), {
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
    return fold((acc, tupledArg) => (acc + (tupledArg[0] * tupledArg[1])), 0, counts);
}

export function distance(input) {
    const tupledArg = splitAndSort(parseInput(input));
    return findDistance(tupledArg[0], tupledArg[1]) | 0;
}

export function productOfInput(input) {
    let tupledArg;
    return productOfOccurances((tupledArg = split(parseInput(input)), countOccurrances(tupledArg[0], tupledArg[1])));
}

