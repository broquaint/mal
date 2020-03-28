// If your target language is statically typed then you will need some
// way for read_form to return a variant or subclass type. For example,
// if your language is object oriented, then you can define a top level
// MalType (in types.qx) that all your mal data types inherit from. The
// MalList type (which also inherits from MalType) will contain a
// list/array of other MalTypes. If your language is dynamically typed
// then you can likely just return a plain list/array of other mal types.

interface MalType

interface MalAtom : MalType

data class MalNumber(val num: Int) : MalAtom

data class MalSymbol(val sym: String) : MalAtom

data class MalBoolean(val bool: Boolean) : MalAtom

open class MalString(val str: String) : MalAtom

// XXX Inheriting from MalString is a bit shonky.
data class MalKeyword(val kw: String) : MalString(kw)

// Would use MalAtom but that's already a thing :/
data class MalCljAtom(var value : MalType) : MalType

class MalNil : MalAtom

interface MalSeq : MalType {
    val atoms : List<MalType>

    val size: Int get() = atoms.size
    fun head()    = atoms[0]
    fun tail()    = MalList(atoms.slice(1 .. atoms.size - 1))
    fun last()    = atoms.last()
    fun butlast() = MalList(atoms.slice(0 .. atoms.size - 2))

    operator fun get(index: Int): MalType = atoms[index]
    operator fun get(index: MalNumber): MalType = atoms[index.num]
    // TODO Maybe implement complementN too?
}

data class MalList(override val atoms: List<MalType>) : MalSeq
data class MalVector(override val atoms: List<MalType>) : MalSeq

class MalMap(val pairs: Map<MalString, MalType>) : MalType {
    operator fun get(k: MalString): MalType = pairs[k] ?: MalNil()
}

typealias MalFn = (MalSeq) -> MalType

abstract class MalCallable(val func: MalFn, var name: String, val meta: MalType) : MalType {
    var isMacro = false

    operator fun invoke(args: MalSeq) = func(args)

    abstract fun withMeta(m: MalType): MalCallable
}

// Allow name to be set after the fact so functions in Env are named.
class MalFunc(func: MalFn, name: String = "anon", meta: MalType = MalNil()) : MalCallable(func, name, meta) {
    override fun withMeta(m: MalType) =
        MalFunc(func, name, m)
}

class MalUserFunc(
    val ast:    MalType,
    val params: MalSeq,
    val env:    Env,
    name:       String,
    meta:       MalType,
    func:       MalFn
) : MalCallable(func, name, meta) {
    override fun withMeta(m: MalType) =
        MalUserFunc(ast, params, env, name, m, func)
}

data class MalUserEx(val src: MalType) : Exception("Exception raised"), MalType
data class MalCoreEx(val msg: String) : Exception(msg)

// Helper functions.
fun emptyMalList() = MalList(listOf())
fun malListOf(vararg elems: MalType) = malListOf(elems.asList())
fun malListOf(elems: List<MalType>) = MalList(elems)
fun malMapOf(elems: List<Pair<MalString, MalType>>) = MalMap(mapOf(*elems.toTypedArray()))
fun malSym(sym: String) = MalSymbol(sym)
fun malFun(name: String, f: MalFn) = MalFunc(f, name)
