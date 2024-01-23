import { MalType } from './types.ts'

export function read_str(input: string): MalType {
    return read_form(new Reader(tokenize(input)))
}

export function tokenize(input: string): Array<string> {
    return []
}

function read_form(r: Reader): MalType {
    return {}
}

class Reader {
    tokens: Array<string>;
    pos = 0;

    constructor(tokens: Array<string>) {
        this.tokens = tokens
    }
}