export type MalList = {
    type: 'list',
    values: Array<MalType>
}

export interface MalAtom {
    type: string,
    value: string | number
}

export type MalSymbol = MalAtom & {
    type: 'symbol'
    value: string
}
export type MalNumber = MalAtom & {
    type: 'number'
    value: number
}

export type MalType = MalList | MalNumber | MalSymbol