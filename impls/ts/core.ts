import { pr_str } from "./printer.ts";
import { MalFunc, MalMap, MalNumber, MalSeq, MalType, mal } from "./types.ts";

const core : { [index: string]: MalFunc } = {
    '+': malCalc((a,b) => a + b),
    '-': malCalc((a,b) => a - b),
    '*': malCalc((a,b) => a * b),
    '/': malCalc((a,b) => a / b),
    // call pr_str on the first parameter with print_readably set to true, prints the result to the screen and then return nil. 
    'prn': mal.func((v: MalType) => {
        console.log(pr_str(v))
        return mal.nil()
    }),
    // take the parameters and return them as a list.
    'list': mal.func((...vals: MalType[]) => {
        return mal.list(vals)
    }),
    // return true if the first parameter is a list, false otherwise.
    'list?': mal.func((v: MalType) => {
        return mal.bool(v.type === 'list')
    }),
    // treat the first parameter as a list and return true if the list is empty and false if it contains any elements.
    'empty?': mal.func((v: MalType) => {
        return mal.bool(v.type === 'list' && v.values.length === 0)
    }),
    // treat the first parameter as a list and return the number of elements that it contains.
    'count': mal.func((v: MalType) => {
        return mal.num(v.type === 'list' ? v.values.length : 0)
    }),
    // compare the first two parameters and return true if they are the same type and contain the same value. In the case of equal length lists, each element of the list should be compared for equality and if they are the same return true, otherwise false.
    '=': mal.func((a: MalType, b: MalType) => {
        return mal.bool(isEqual(a, b))
    }),
    // treat the first two parameters as numbers and do the corresponding numeric comparison, returning either true or false.
    '<': malComp((a,b) => a < b),
    '<=': malComp((a,b) => a <= b),
    '>': malComp((a,b) => a > b),
    '>=': malComp((a,b) => a >= b)
}

export default core

function malCalc(f: (x: number, y: number) => number): MalFunc {
    return mal.func((a: MalType, b: MalType) => {
        const x = a as MalNumber
        const y = b as MalNumber
        return mal.num(f(x.value, y.value))
    })
}

function malComp(f: (x: number, y: number) => boolean): MalFunc {
    return mal.func((a: MalType, b: MalType) => {
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
