export type MalList = {
    type: 'list',
    values: Array<MalType>
}

export type MalSymbol = {
    type: 'symbol'
    value: string
}
export type MalNumber = {
    type: 'number'
    value: number
}
export type MalFuncSig = (...vals: MalType[]) => MalType
export type MalFunc = {
    type: 'function',
    value: MalFuncSig
}
export type MalType = MalList | MalNumber | MalSymbol | MalFunc

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
    }
}