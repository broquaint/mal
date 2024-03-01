import { MalSymbol, MalType, MalSeq } from "./types.ts";

export default class Env {
    outer: Env | null;
    data: { [index: string]: MalType };

    constructor(outer?: Env, binds?: MalSeq, exprs?: MalSeq) {
        this.outer = outer ?? null;
        this.data  = {}
        if(binds && exprs) {
            binds.values.forEach((v, idx) => {
                const sym = v as MalSymbol
                this.set(sym, exprs.values[idx])
            })
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