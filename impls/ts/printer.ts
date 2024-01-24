import { MalType } from './types.ts'

export function pr_str(v: MalType): string {
    switch(v.type) {
        case 'list':
            return "(" + v.values.map(pr_str).join(" ") + ")"
        case 'number':
            return String(v.value)
        case 'symbol':
            return v.value
        default:
            throw "Unrecognised value type " + v
    }
}