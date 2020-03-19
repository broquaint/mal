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

// Would use MalAtom but that's already a thing :/
data class MalCljAtom(var value : MalType) : MalType

class MalNil() : MalAtom

interface MalSeq : MalType {
    val atoms : List<MalType>

    val size: Int get() = atoms.size
    fun head()    = atoms[0]
    fun tail()    = MalList(atoms.slice(1 .. atoms.size - 1))
    fun last()    = atoms.last()
    fun butlast() = MalList(atoms.slice(0 .. atoms.size - 2))

    operator fun get(index: Int): MalType = atoms[index]
    // TODO Maybe implement complementN too?
}

class MalList(override val atoms : List<MalType>) : MalSeq
class MalVector(override val atoms : List<MalType>) : MalSeq

// Allow name to be set after the fact so functions in Env are named.
class MalFunc(val func : (MalSeq) -> MalType, var name : String = "anon") : MalType {
    operator fun invoke(args: MalSeq) : MalType {
        return func(args)
    }
}

class MalUserFunc(
    val ast    : MalType,
    val params : MalSeq,
    val env    : Env,
    val fn     : MalFunc
) : MalType

// Helper functions.
fun emptyMalList() = MalList(listOf())
fun malListOf(vararg elems: MalType) = MalList(elems.asList())
fun malListOf(elems: List<MalType>) = MalList(elems)
fun malSym(sym: String) = MalSymbol(sym)
fun malFun(name: String, f: (MalSeq) -> MalType) = MalFunc(f, name)
