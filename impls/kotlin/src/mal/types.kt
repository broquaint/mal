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

interface MalKey : MalType

data class MalString(val str: String) : MalKey
data class MalKeyword(val kw: String) : MalKey

// Would use MalAtom but that's already a thing :/
data class MalCljAtom(var value : MalType) : MalType

class MalNil : MalAtom

interface MalMeta {
    val meta: MalType
    fun withMeta(m: MalType): MalType
}

interface MalSeq : MalMeta, MalType {
    val atoms: List<MalType>

    val size: Int get() = atoms.size
    fun head()    = atoms[0]
    fun tail()    = MalList(atoms.slice(1 .. atoms.size - 1))
    fun last()    = atoms.last()
    fun butlast() = MalList(atoms.slice(0 .. atoms.size - 2))

    operator fun get(index: Int): MalType = atoms[index]
    operator fun get(index: MalNumber): MalType = atoms[index.num]
    // TODO Maybe implement complementN too?
}

data class MalList(override val atoms: List<MalType>, override val meta: MalType = MalNil()) : MalSeq {
    // XXX Not technically a copy ...
    override fun withMeta(m: MalType) =
        MalList(atoms, m)
}
data class MalVector(override val atoms: List<MalType>, override val meta: MalType = MalNil()) : MalSeq {
    // XXX Not technically a copy ...
    override fun withMeta(m: MalType) =
        MalVector(atoms, m)
}

class MalMap(val pairs: Map<MalKey, MalType>, override val meta: MalType = MalNil()) : MalMeta, MalType {
    operator fun get(k: MalKey): MalType = pairs[k] ?: MalNil()

    // XXX Not technically a copy ...
    override fun withMeta(m: MalType) =
        MalMap(pairs, m)
}

typealias MalFn = (MalSeq) -> MalType

abstract class MalCallable(val func: MalFn, var name: String, override val meta: MalType) : MalMeta, MalType {
    var isMacro = false

    operator fun invoke(args: MalSeq) = func(args)
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

data class MalUserEx(val src: MalType) : Exception("Exception raised"), MalType {
    constructor(msg: String) : this(MalString(msg))
}
data class MalCoreEx(val msg: String) : Exception(msg)

// Helper functions.
fun emptyMalList() = MalList(listOf())
fun malListOf(vararg elems: MalType) = malListOf(elems.asList())
fun malListOf(elems: List<MalType>) = MalList(elems)
fun malMapOf(elems: List<Pair<MalKey, MalType>>) = MalMap(mapOf(*elems.toTypedArray()))
fun malSym(sym: String) = MalSymbol(sym)
fun malFun(name: String, f: MalFn) = MalFunc(f, name)