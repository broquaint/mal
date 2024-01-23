import { MalType } from './types.ts'
import { read_str } from './reader.ts'
import { pr_str } from './printer.ts'

function READ(s: string): MalType {
    return read_str(s)
}
function EVAL(v: MalType): MalType {
    return v
}
function PRINT(v: MalType): string {
    return pr_str(v)
}
function rep(s: string): string {
    return PRINT(EVAL(READ(s)))
}

let line = prompt('user>')
while (line !== null) {
  console.log(rep(line));
  line = prompt('user>')
}