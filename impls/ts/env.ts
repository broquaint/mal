import { MalMapValue, MalType } from "./types.ts";

export default class Env {
    outer: Env | undefined;
    data: MalMapValue;

    constructor(outer?: Env) {
        this.outer = outer;
        this.data  = {}
    }

    set(k: string, v: MalType): MalType {
        this.data[k] = v
        return v
    }

    find(k: string): Env | null {
        if(k in this.data)
            return this
        else
            if(this.outer)
                return this.outer.find(k)
            else
                return null
    }

    get(k: string): MalType {
        const e = this.find(k);
        if(e !== null)
            return e.data[k]
        else
            throw `The symbol '${k}' not found in the environment`
    }
}