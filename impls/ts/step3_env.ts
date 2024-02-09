import { MalNumber, MalList, MalMap, MalType, mal, MalFunc, MalSymbol } from './types.ts'
import { read_str } from './reader.ts'
import { pr_str } from './printer.ts'
import Env from './env.ts';

function eval_ast(ast: MalType, env: Env): MalType {
    switch(ast.type) {
        case 'symbol': {
            return env.get(ast)
        }
        case 'list':
        case 'vector': {
            const valGen = mal[ast.type]
            const seed = valGen([])
                return ast.values.reduce(
                    (a: typeof seed, b: MalType) => valGen(a.values.concat([EVAL(b, env)])),
                    seed
                )
            }
        case 'map': {
            return Array.from(ast.values.entries()).reduce(
                (acc: MalMap, [k, v]) => mal.map(acc.values.set(k, EVAL(v, env))),
                mal.map(new Map())
            )
        }
        default:
            return ast
    }
}

function READ(s: string): MalType {
    return read_str(s)
}
function EVAL(v: MalType, env: Env): MalType {
    if(v.type != 'list') {
        return eval_ast(v, env)
    }
    else if(v.values.length === 0) {
        return v
    }
    else {
        const head = v.values[0]
        const tail = v.values.slice(1)

        if(head.type === 'symbol') {
            switch(head.value) {
                case 'def!': {
                    const v = EVAL(tail[1], env)
                    return env.set((tail[0] as MalSymbol), v)
                }
                case 'let*': {
                    return EVAL(tail[1], makeEnv((tail[0] as MalList), env))
                }
            }
        }

        const callSite = eval_ast(v, env) as MalList
        const f        = callSite.values[0] as MalFunc
        const args     = callSite.values.slice(1)

        return f.value.apply(null, args)
    }
}
function PRINT(v: MalType): string {
    return pr_str(v)
}
function rep(s: string): string {
    return PRINT(EVAL(READ(s), env))
}

function makeEnv(pairs: MalList, outerEnv: Env): Env {
    const newEnv = new Env(outerEnv)
    for(let idx = 0; idx < pairs.values.length; idx += 2) {
        const k = pairs.values[idx] as MalSymbol
        const v = pairs.values[idx + 1]
        newEnv.set(k, EVAL(v, newEnv))
    }
    return newEnv
}

function malCalc(f: (x: number, y: number) => number): MalFunc {
    return mal.func((a: MalType, b: MalType) => {
        const x = a as MalNumber
        const y = b as MalNumber
        return mal.num(f(x.value, y.value))
    })
}

const env = makeEnv(
    mal.list([
        mal.symbol('+'), malCalc((a,b) => a + b),
        mal.symbol('-'), malCalc((a,b) => a - b),
        mal.symbol('*'), malCalc((a,b) => a * b),
        mal.symbol('/'), malCalc((a,b) => a / b),
    ]),
    new Env()
)

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
