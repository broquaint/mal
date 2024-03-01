import { MalType, EscapeMap, MalMapValue, MalKey } from './types.ts'

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
            return print_readably ? `"${readable_string(v.value)}"`: v.value
        case 'keyword':
        case 'symbol':
            return v.value
        case 'bool':
            return String(v.value)
        case 'nil':
            return 'nil'
        case 'function':
            return '#<function>'
        default:
            throw "Unrecognised value type " + JSON.stringify(v)
    }
}

const unescapeMap: EscapeMap = { '\\': '\\\\', "\n": '\\n', '"': '\\"' }
const unescapeRe = /\\|\n|"/g

function readable_string(value: string): string {
    return value.replace(unescapeRe, c => unescapeMap[c])
}

function readable_map(values: MalMapValue, print_readably: boolean): string {
    function keyToPrintable(k: MalKey): string {
        return ((k.type == 'keyword' ? ':' : '') + k.value)
    }
    return Array.from(values.entries()).reduce<string[]>(
        (acc, [k, v]) => acc.concat([keyToPrintable(k), pr_str(v, print_readably)]),
        []
    ).join(' ')
}
