import { MalList, MalMap, MalType, mal, MalSymbol, MalSeq } from './types.ts'
import { read_str } from './reader.ts'
import { pr_str } from './printer.ts'
import Env from './env.ts';
import core from './core.ts';

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
function EVAL(ast: MalType, env: Env): MalType {
    tco: while(true) {
        if(ast.type != 'list') {
            return eval_ast(ast, env)
        }
        else if(ast.values.length === 0) {
            return ast
        }
        else {
            const head = ast.values[0]
            const tail = ast.values.slice(1)

            if(head.type === 'symbol') {
                switch(head.value) {
                    case 'def!': {
                        const v = EVAL(tail[1], env)
                        return env.set((tail[0] as MalSymbol), v)
                    }
                    case 'let*': {
                        env = makeEnv((tail[0] as MalList), env)
                        ast = ast.values[2]
                        continue tco
                    }
                    case 'do': {
                        const result = eval_ast(mal.list(tail), env) as MalList
                        ast = result.values.pop()!  
                        continue tco
                    }
                    case 'if': {
                        const guard = EVAL(tail[0], env)
                        ast = isTrue(guard)   ? tail[1] :
                              tail.length > 2 ? tail[2] : mal.nil()
                        continue tco
                    }
                    case 'fn*': {
                        const binds = tail[0] as MalSeq
                        const body  = tail[1]
                        return mal.malfunc(body, binds, env)
                    }
                }
            }

            const callSite = eval_ast(ast, env) as MalList
            const f        = callSite.values[0]
            const args     = callSite.values.slice(1)

            switch(f.type) {
                case 'function':
                    return f.value.apply(null, args)
                case 'malfunc':
                    ast = f.ast
                    env = new Env(f.env, f.params, mal.list(args))
                    continue tco
                default:
                    throw `Don't know how to call: ${f.type}`
            }
        }
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

function isTrue(v: MalType): boolean {
    return !(v.type == 'bool' && !v.value || v.type == 'nil')
}

const env = new Env()
Object.entries(core).forEach(([s, f]) => {
    env.set(mal.symbol(s), f)
})

rep("(def! not (fn* (a) (if a false true)))")

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
