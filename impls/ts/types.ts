export type MalList = {
    type: 'list',
    values: Array<MalType>
}
export type MalVector = {
    type: 'vector',
    values: Array<MalType>
}

export type MalKey = MalKeyword | MalString
export type MalMapValue = Map<MalKey, MalType>
export type MalMap = {
    type: 'map',
    values: MalMapValue
}

export type MalSymbol = {
    type: 'symbol'
    value: string
}
export type MalNumber = {
    type: 'number'
    value: number
}
export type MalString = {
    type: 'string',
    value: string
}
export type MalBool = {
    type: 'bool',
    value: boolean
}
export type MalKeyword = {
    type: 'keyword',
    value: string
}

export type MalNil = {
    type: 'nil'
}
export type MalFuncSig = (...vals: MalType[]) => MalType
export type MalFunc = {
    type: 'function',
    value: MalFuncSig
}
export type MalType = MalList | MalVector | MalMap
          | MalNumber | MalString | MalBool | MalKeyword | MalNil | MalSymbol
          | MalFunc

// Still used by step2_eval, unused otherwise
export type MalEnv = {
    [index: string]: MalType
}

// Helper function to take Mal–adjacent values and turn them into proper Mal–typed objects.
export const mal = {
    num: function(n: number): MalNumber {
        return { type: 'number', value: n }
    },
    func: function(f: MalFuncSig): MalFunc {
        return { type: 'function', value: f }
    },
    list: function(v: Array<MalType>): MalList {
        return { type: 'list', values: v }
    },
    vector: function(v: Array<MalType>): MalVector {
        return { type: 'vector', values: v }
    },
    map: function(v: MalMapValue): MalMap {
        return { type: 'map', values: v }
    },
    nil: function(): MalNil {
        return { type: 'nil' }
    },
    symbol: function(v: string): MalSymbol {
        return { type: 'symbol', value: v }
    }
}

// Used for escaping/reading + unescaping/printing
export type EscapeMap = {
    readonly [index: string]: string;
}