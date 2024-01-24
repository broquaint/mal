import { MalType, MalList, MalNumber, MalSymbol } from './types.ts'

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
        case '(': return read_list(r)
        default: return read_atom(r)
    }
}

function read_list(r: Reader): MalList {
    r.next() // Move past opening paren
    const list : Array<MalType> = []
    while(r.peek() != ")") {
        list.push(read_form(r))
    }
    r.next() // Move past closing paren
    return { type: 'list', values: list }
}

function read_atom(r: Reader): MalNumber | MalSymbol {
    const v = r.next()

    // This is lazy as JavaScript numbers != Mal numbers but it will do for now.
    if(!Number.isNaN(Number(v)))
        return { type: 'number', value: Number(v) }
    else
        return { type: 'symbol', value: v }
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
        if(this.pos <= this.tokens.length)
            return this.tokens[this.pos]
        else
            throw "Reached the end of the input, unbalanced paren?"
    }
}