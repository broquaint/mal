fun int_ops_reducer(f: (Int, Int) -> Int, args: MalList): MalNumber =
    args.atoms.map { v: MalType -> v as MalNumber }
              .reduce { acc, v -> MalNumber(f(acc.num, v.num)) }

fun to_fun(name: String, f: (MalList) -> MalType) : Pair<MalSymbol, MalFunc> =
    malSym(name) to malFun(name, f)

// =: compare the first two parameters and return true if they are the same type and contain the same value.
fun is_equal(a: MalType, b: MalType) =
    if(a::class == b::class) {
        when(a) {
            is MalNumber  -> a.num  == (b as MalNumber).num
            is MalString  -> a.str  == (b as MalString).str
            is MalSymbol  -> a.sym  == (b as MalSymbol).sym
            is MalBoolean -> a.bool == (b as MalBoolean).bool
            is MalNil     -> true
            is MalList    -> compare_lists(a, (b as MalList))
            is MalFunc    -> a.func == (b as MalFunc).func
            else -> throw Exception("Unknown type $a in is_equal (aka =)")
        }
    }
    else {
        false
    }
// In the case of equal length lists, each element of the list should be compared for equality and if they are the same return true, otherwise false.
fun compare_lists(a: MalList, b: MalList) : Boolean {
    if(a.atoms.count() == b.atoms.count())
      return a.atoms.indices.all { v: Int -> is_equal(a.atoms[v], b.atoms[v]) }
    else
      return false
}

object core {
    val ns : Map<MalSymbol, MalFunc> = mutableMapOf(
        // Basic number ops.
        malSym("+") to malFun("plus")  { int_ops_reducer(Int::plus,  it) },
        malSym("-") to malFun("minus") { int_ops_reducer(Int::minus, it) },
        malSym("*") to malFun("times") { int_ops_reducer(Int::times, it) },
        malSym("/") to malFun("div")   { int_ops_reducer(Int::div,   it) },

        // prn: call pr_str on the first parameter with print_readably set to true, prints the result to the screen and then return nil. Note that the full version of prn is a deferrable below.
        to_fun("prn") { pr_str(it[0]); MalNil() },
        // list: take the parameters and return them as a list.
        to_fun("list") { it }, // we always get a list at this point
        // list?: return true if the first parameter is a list, false otherwise.
        to_fun("list?") { MalBoolean(it[0] is MalList) },
        // empty?: treat the first parameter as a list and return true if the list is empty and false if it contains any elements.
        to_fun("empty?") { MalBoolean(it[0] is MalList && (it[0] as MalList).atoms.isEmpty()) },
        // count: treat the first parameter as a list and return the number of elements that it contains.
        to_fun("count") { MalNumber((it[0] as MalList).atoms.count()) },

        // str: calls pr_str on each argument with print_readably set to false, concatenates the results together ("" separator), and returns the new string.
        to_fun("str") {
            MalString(it.atoms.map { pr_str(it) }.joinToString(""))
        },        

        // =: see is_equal
        malSym("=") to malFun("equals?") {
            val a = it[0]
            val b = it[1]
            MalBoolean(is_equal(a, b))
        },
        // <, <=, >, and >=: treat the first two parameters as numbers and do the corresponding numeric comparison, returning either true or false.
        malSym("<") to malFun("lt") {
            val a = it[0] as MalNumber
            val b = it[1] as MalNumber
            MalBoolean(a.num < b.num)
        },
        malSym("<=") to malFun("lt-eq") {
            val a = it[0] as MalNumber
            val b = it[1] as MalNumber
            MalBoolean(a.num <= b.num)
        },
        malSym(">") to malFun("gt") {
            val a = it[0] as MalNumber
            val b = it[1] as MalNumber
            MalBoolean(a.num > b.num)
        },
        malSym(">=") to malFun("gt-eq") {
            val a = it[0] as MalNumber
            val b = it[1] as MalNumber
            MalBoolean(a.num >= b.num)
        },

        to_fun("read-string") {
            read_str((it[0] as MalString).str)
        },

        to_fun("slurp") {
            MalString(java.io.File((it[0] as MalString).str).readText())
        },

        to_fun("atom") {
            MalCljAtom(it[0])
        },
        to_fun("atom?") {
            MalBoolean(it[0] is MalCljAtom)
        },
        to_fun("deref") {
            (it[0] as MalCljAtom).value
        },
        to_fun("reset!") {
            val a = it[0] as MalCljAtom
            a.value = it[1]
            a.value
        },
        to_fun("swap!") {
            val atom     = it[0] as MalCljAtom
            val callable = it[1]
            val fn       = if(callable is MalUserFunc) callable.fn else callable as MalFunc
            // Pull out args if there are any.
            val args = it.atoms.slice(2 .. (if(it.size > 2) it.size - 1 else 1))
            // Call the function with atom value + any args.
            val res  = fn(malListOf(listOf(atom.value) + args))
            atom.value = res
            res
        },

        to_fun("cons") {
            val rest = if(it.size > 1) it[1] as MalList else emptyMalList()
            malListOf(listOf(it.head()) + rest.atoms)
        },
        to_fun("concat") {
            malListOf(it.atoms.flatMap { (it as MalList).atoms })
        }
    )
}
