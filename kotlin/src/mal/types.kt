// If your target language is statically typed then you will need some
// way for read_form to return a variant or subclass type. For example,
// if your language is object oriented, then you can define a top level
// MalType (in types.qx) that all your mal data types inherit from. The
// MalList type (which also inherits from MalType) will contain a
// list/array of other MalTypes. If your language is dynamically typed
// then you can likely just return a plain list/array of other mal types.

interface MalType {}

interface MalAtom : MalType {}

data class MalNumber(val num : Int) : MalAtom

data class MalString(val str : String) : MalAtom

data class MalSymbol(val sym : String) : MalAtom

data class MalBoolean(val bool : Boolean) : MalAtom

class MalNil() : MalAtom

open class MalList(val atoms : List<MalType>) : MalType {
    fun head()    = atoms[0]
    fun tail()    = MalList(atoms.slice(1 .. atoms.size - 1))
    fun last()    = atoms.last()
    fun butlast() = MalList(atoms.slice(0 .. atoms.size - 2))
    operator fun get(index: Int): MalType = atoms[index]
    // TODO Maybe implement complementN too.
}

class MalVector(atoms : List<MalType>) : MalList(atoms)

class MalFunc(val func : (MalList) -> MalType, val name : String = "anon") : MalType {
    operator fun invoke(args: MalList) : MalType {
        return func(args)
    }
}

class MalUserFunc(
    val ast    : MalType,
    val params : MalList,
    val env    : Env,
    val fn     : MalFunc
) : MalType

// Helper functions.
fun emptyMalList() = MalList(listOf())
fun malListOf(vararg elems: MalType) = MalList(elems.asList())
fun malSym(sym: String) = MalSymbol(sym)
fun malFun(name: String, f: (MalList) -> MalType) = MalFunc(f, name)
