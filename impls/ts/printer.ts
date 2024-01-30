import { MalType } from './types.ts'

export function pr_str(v: MalType, print_readably = true): string {
    switch(v.type) {
        case 'list':
            return "(" + v.values.map(mv => pr_str(mv, print_readably)).join(" ") + ")"
        case 'number':
            return String(v.value)
        case 'string':
            // console.error(v.value)
            return `"${print_readably ? readable_string(v.value): v.value}"`
        case 'symbol':
            return v.value
        case 'bool':
            return String(v.value)
        case 'nil':
            return 'nil'
        default:
            throw "Unrecognised value type " + v
    }
}

// I wonder if there's a more idiomatic approach to this?
interface EscapeMap {
    readonly [index: string]: string;
}
const unescapeMap: EscapeMap = { '\\': '\\\\', "\n": '\\n', '"': '\\"' }

const unescapeRe = /\\|\n|"/g

function readable_string(value: string): string {
    return value.replace(unescapeRe, c => unescapeMap[c])
}
