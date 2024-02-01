import { MalType, EscapeMap, MalMapValue } from './types.ts'

export function pr_str(v: MalType, print_readably = true): string {
    switch(v.type) {
        case 'list':
            return "(" + v.values.map(mv => pr_str(mv, print_readably)).join(" ") + ")"
        case 'vector':
            return "[" + v.values.map(mv => pr_str(mv, print_readably)).join(" ") + "]"
        case 'map':
            return '{' + readable_map(v.values, print_readably) + "}"
        case 'number':
            return String(v.value)
        case 'string':
            return `"${print_readably ? readable_string(v.value): v.value}"`
        case 'keyword':
            return v.value.replace(/^🔑/, ':')
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

const unescapeMap: EscapeMap = { '\\': '\\\\', "\n": '\\n', '"': '\\"' }

const unescapeRe = /\\|\n|"/g

function readable_string(value: string): string {
    return value.replace(unescapeRe, c => unescapeMap[c])
}

function readable_map(values: MalMapValue, print_readably: boolean): string {
    function keyToPrintable(k: string) {
        return k.indexOf('🔑') === 0 ? k.replace(/^🔑/, ':') : `"${k}"`
    }
    // Need to handle map keys especially as we use regular old strings
    // to create a map–like object. 
    return Object.entries(values).reduce(
        (acc, [k, v]) => acc + keyToPrintable(k) + ' ' + pr_str(v, print_readably),
        ''
    )
}
