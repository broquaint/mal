import { MalSymbol, MalType } from "./types.ts";

export default class Env {
    outer: Env | null;
    data: { [index: string]: MalType };

    constructor(outer?: Env) {
        this.outer = outer ?? null;
        this.data  = {}
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