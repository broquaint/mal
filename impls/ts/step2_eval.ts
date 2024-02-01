import { MalNumber, MalList, MalEnv, MalType, mal, MalFunc } from './types.ts'
import { read_str } from './reader.ts'
import { pr_str } from './printer.ts'

function malCalc(f: (x: number, y: number) => number): MalFunc {
    return mal.func((a: MalType, b: MalType) => {
        const x = a as MalNumber
        const y = b as MalNumber
        return mal.num(f(x.value, y.value))
    })
}

const env: MalEnv = {
    '+': malCalc((a,b) => a + b),
    '-': malCalc((a,b) => a - b),
    '*': malCalc((a,b) => a * b),
    '/': malCalc((a,b) => a / b),
}

function eval_ast(ast: MalType, env: MalEnv): MalType {
    switch(ast.type) {
        case 'symbol': {
            if(ast.value in env)
                return env[ast.value]
            else
                throw `Cannot find symbol '${ast.value}' in environment`
        }
        case 'list': {
            return ast.values.reduce(
                (a: MalList, b: MalType) => mal.list(a.values.concat([EVAL(b, env)])),
                mal.list([])
            );
        }
        default:
            return ast
    }
}

function READ(s: string): MalType {
    return read_str(s)
}
function EVAL(v: MalType, env: MalEnv): MalType {
    if(v.type != 'list') {
        return eval_ast(v, env)
    }
    else if(v.values.length === 0) {
        return v
    }
    else {
        const ml   = eval_ast(v, env) as MalList
        const f    = ml.values[0] as MalFunc
        const args = ml.values.slice(1)
        return f.value.apply(null, args)
    }
}
function PRINT(v: MalType): string {
    return pr_str(v)
}
function rep(s: string): string {
    return PRINT(EVAL(READ(s), env))
}

let line = prompt('user>')
while (line !== null) {
    try {
        console.log(rep(line));
    }
    catch(e) {
        console.log('Error: ', e)
    }
    line = prompt('user>')
}