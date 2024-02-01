import { MalType, MalList, MalNumber, MalSymbol, MalBool, MalKeyword, MalNil, MalString, EscapeMap, MalMap, MalVector, MalMapValue } from './types.ts'

// No /x flag on JavaScript regexps ;_;
const tokenize_re = /(~@|[\[\]{}()'\`~^@]|"(?:\\.|[^\\"])*"?|;.*|[^\s\[\]{}('"`,;)]+)/g

export function read_str(input: string): MalType {
    return read_form(new Reader(tokenize(input)))
}

export function tokenize(input: string): Array<string> {
    const result = input.match(tokenize_re)
    if(result !== null)
        return result.map(s => s.trim())
            .filter(s => s.length > 0 && !s.startsWith(';'))
    else
        return []
}

function read_form(r: Reader): MalType {
    switch(r.peek()) {
        case '(':
        case '[':
        case '{':
            return read_list(r, r.peek())
        default: return read_atom(r)
    }
}

const seqPairMap: { readonly [index: string]: string } = { '(': ')', '[': ']', '{': '}' }

function read_list(r: Reader, c: string): MalList | MalVector | MalMap {
    r.next() // Move past opening paren
    const list : Array<MalType> = []
    while(r.peek() != seqPairMap[c]) {
        list.push(read_form(r))
    }
    r.next() // Move past closing paren
    return make_sequence(c, list)
}

function make_sequence(c: string, list: MalType[]): MalList | MalVector | MalMap {
    switch(c) {
        case '(':
            return { type: 'list', values: list }
        case '[':
            return { type: 'vector', values: list }
        case '{': {
            if(list.length % 2 !== 0)
                throw "Encountered map with uneven number of values, aborting"
            const newMap: MalMapValue = {}
            for(let i = 0; i < list.length; i += 2) {
                const [k,v] = list.slice(i, i+2)
                if(k.type === 'string' || k.type === 'keyword')
                    newMap[k.value] = v
                else
                    throw `Cannot use "${JSON.stringify(v)}" as map key, aborting`
            }
            return { type: 'map', values: newMap }
        }
        default:
            throw `Tried to make a sequence from '${c}' which should be impossible!`
    }
}

function read_atom(r: Reader): MalNumber | MalSymbol | MalString | MalKeyword | MalBool | MalNil {
    const v = r.next()

    // This is lazy as JavaScript numbers != Mal numbers but it will do for now.
    if(!Number.isNaN(Number(v)))
        return { type: 'number', value: Number(v) }
    else if(v === 'true' || v === 'false')
        return { type: 'bool', value: Boolean(v === 'true') }
    else if(v === 'nil')
        return { type: 'nil' }
    else if(v.indexOf('"') === 0)
        return read_string_value(v)
    else if(v.indexOf(':') === 0)
        return { type: 'keyword', value: '🔑' + v.substring(1) }
    else
        return { type: 'symbol', value: v }
}

const escapeMap: EscapeMap = { '\\': '\\', 'n': "\n", '"': '"' }

function read_string_value(v: string): MalString {
    if(!v.match(/"$/) || v.length === 1)
        throw 'Unexpected end of input, unbalanced quote?'
    // This is insufficient to pass the some tests but I'm insufficiently bothered to fix it.
    if(v.match(/[^\\]\\"$/))
        throw 'Unexpected end of input, unbalanced escape?'
    const sv = v.replace(/^"|"$/g, '')
        .replace(/\\(.)/g, (_, c) => c in escapeMap ? escapeMap[c] : c)
    return { type: 'string', value: sv }
}

class Reader {
    tokens: Array<string>;
    pos = 0;

    constructor(tokens: Array<string>) {
        this.tokens = tokens
    }

    next(): string {
        const tok = this.peek()
        this.pos++
        return tok
    }

    peek(): string {
        if(this.pos < this.tokens.length)
            return this.tokens[this.pos]
        else
            throw "Reached the end of the input, unbalanced paren?"
    }
}
