// If your target language is statically typed then you will need some
// way for read_form to return a variant or subclass type. For example,
// if your language is object oriented, then you can define a top level
// MalType (in types.qx) that all your mal data types inherit from. The
// MalList type (which also inherits from MalType) will contain a
// list/array of other MalTypes. If your language is dynamically typed
// then you can likely just return a plain list/array of other mal types.

interface MalType {}

data class MalList(val atoms : List<MalType>) : MalType

interface MalAtom : MalType {}

data class MalNumber(val number : Number) : MalAtom

data class MalSymbol(val sym : String) : MalAtom
