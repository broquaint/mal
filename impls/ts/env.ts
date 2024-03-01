import { MalSymbol, MalType, MalSeq, mal } from "./types.ts";

export default class Env {
    outer: Env | null;
    data: { [index: string]: MalType };

    constructor(outer?: Env, binds?: MalSeq, exprs?: MalSeq) {
        this.outer = outer ?? null;
        this.data  = {}
        if(binds && exprs) {
            for(let idx = 0; idx < binds.values.length; idx++) {
                const sym = binds.values[idx] as MalSymbol
                if(sym.value === '&') {
                    const lastBind = binds.values[idx + 1] as MalSymbol
                    this.set(lastBind, mal.list(exprs.values.slice(idx)))
                    break;
                }
                else {
                    this.set(sym, exprs.values[idx])
                }
            }
        }
    }

    set(k: MalSymbol, v: MalType): MalType {
        this.data[k.value] = v
        return v
    }

    find(k: MalSymbol): Env | null {
        if(k.value in this.data)
            return this
        else
            if(this.outer)
                return this.outer.find(k)
            else
                return null
    }

    get(k: MalSymbol): MalType {
        const env = this.find(k)
        if(env !== null)
            return env.data[k.value]!
        else
            // Slightly awkward error phrasing to satisfy tests.
            throw `The symbol '${k.value}' not found in the environment`
    }
}