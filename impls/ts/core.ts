import { pr_str } from "./printer.ts";
import { MalFunc, MalMap, MalNumber, MalSeq, MalType, mal } from "./types.ts";

const core : { [index: string]: MalFunc } = {
    '+': malCalc((a,b) => a + b),
    '-': malCalc((a,b) => a - b),
    '*': malCalc((a,b) => a * b),
    '/': malCalc((a,b) => a / b),
    // take the parameters and return them as a list.
    'list': mal.function((...vals: MalType[]) => {
        return mal.list(vals)
    }),
    // return true if the first parameter is a list, false otherwise.
    'list?': mal.function((v: MalType) => {
        return mal.bool(v.type === 'list')
    }),
    // treat the first parameter as a list and return true if the list is empty and false if it contains any elements.
    'empty?': mal.function((v: MalType) => {
        return mal.bool(isMalSeq(v) ? v.values.length === 0 : false)
    }),
    // treat the first parameter as a list and return the number of elements that it contains.
    'count': mal.function((v: MalType) => {
        return mal.number(isMalSeq(v) ? v.values.length : 0)
    }),
    // compare the first two parameters and return true if they are the same type and contain the same value. In the case of equal length lists, each element of the list should be compared for equality and if they are the same return true, otherwise false.
    '=': mal.function((a: MalType, b: MalType) => {
        return mal.bool(isEqual(a, b))
    }),
    // treat the first two parameters as numbers and do the corresponding numeric comparison, returning either true or false.
    '<': malComp((a,b) => a < b),
    '<=': malComp((a,b) => a <= b),
    '>': malComp((a,b) => a > b),
    '>=': malComp((a,b) => a >= b),
    // calls pr_str on each argument with print_readably set to true, joins the results with " " and returns the new string.
    'pr-str': mal.function((...vals: MalType[]) => {
        return mal.string(vals.map(v => pr_str(v)).join(' '))
    }),
    // pr_str on each argument with print_readably set to false, concatenates the results together ("" separator), and returns the new string
    'str': mal.function((...vals: MalType[]) => {
        return mal.string(vals.map(v => pr_str(v, false)).join(''))
    }),
    // calls pr_str on each argument with print_readably set to true, joins the results with " ", prints the string to the screen and then returns nil.
    'prn': mal.function((...vals: MalType[]) => {
        console.log(vals.map(v => pr_str(v)).join(' '))
        return mal.nil()
    }),
    // pr_str on each argument with print_readably set to false, concatenates the results together ("" separator), and returns the new string
    'println': mal.function((...vals: MalType[]) => {
        console.log(vals.map(v => pr_str(v, false)).join(' '))
        return mal.nil()
    }),
}

export default core

function isMalSeq(v: MalType): v is MalSeq {
    return v.type === 'list' || v.type === 'vector'
}

function malCalc(f: (x: number, y: number) => number): MalFunc {
    return mal.function((a: MalType, b: MalType) => {
        const x = a as MalNumber
        const y = b as MalNumber
        return mal.number(f(x.value, y.value))
    })
}

function malComp(f: (x: number, y: number) => boolean): MalFunc {
    return mal.function((a: MalType, b: MalType) => {
        const x = a as MalNumber
        const y = b as MalNumber
        return mal.bool(f(x.value, y.value))
    })
}

function isEqual(a: MalType, b: MalType): boolean {
    switch(a.type) {
        case 'symbol':
        case 'number':
        case 'string':
        case 'bool':
        case 'keyword':
        case 'function':
            if(a.type === b.type)
                return a.value === b.value
            else
                return false
        case 'list':
        case 'vector':
            if(b.type === 'list' || b.type === 'vector')
                return compare_lists(a, b as MalSeq)
            else
                return false
        case 'map':
            if(a.type === b.type)
                return compare_maps(a, b as MalMap)
            else
                return false
        case 'nil':
            return a.type === b.type
        default:
            return false
    }
}

function compare_lists(a: MalSeq, b: MalSeq): boolean {
    return a.values.length === b.values.length &&
        a.values.every((v, idx) => isEqual(v, b.values[idx]))
}

function compare_maps(a: MalMap, b: MalMap): boolean {
    return a.values.size === b.values.size &&
        Array.from(a.values.entries()).every(([k,v]) => isEqual(v, b.values.get(k)!))
}
