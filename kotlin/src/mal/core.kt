private fun int_ops_reducer(f: (Int, Int) -> Int, args: MalSeq): MalNumber =
    args.atoms.map { v: MalType -> v as MalNumber }
              .reduce { acc, v -> MalNumber(f(acc.num, v.num)) }

private fun to_fun(name: String, f: MalFn) : Pair<MalSymbol, MalFunc> =
    malSym(name) to malFun(name, f)

// =: compare the first two parameters and return true if they are the same type and contain the same value.
private fun is_equal(a: MalType, b: MalType) =
    if(a::class == b::class) {
        when(a) {
            is MalNumber  -> a.num  == (b as MalNumber).num
            is MalString  -> a.str  == (b as MalString).str
            is MalSymbol  -> a.sym  == (b as MalSymbol).sym
            is MalKeyword -> a.kw   == (b as MalKeyword).kw
            is MalBoolean -> a.bool == (b as MalBoolean).bool
            is MalNil     -> true
            is MalSeq     -> compare_lists(a, (b as MalSeq))
            is MalMap     -> compare_maps(a, (b as MalMap))
            is MalFunc    -> a.func == (b as MalFunc).func
            // XXX Not particularly useful but is_equal(a.src, b.src) hits:
            // error: type checking has run into a recursive problem
            is MalUserEx  -> a.src == (b as MalUserEx).src
            else -> throw MalCoreEx("Unknown type $a in is_equal (aka =)")
        }
    }
    else {
        false
    }
// In the case of equal length lists, each element of the list should be compared for equality and if they are the same return true, otherwise false.
private fun compare_lists(a: MalSeq, b: MalSeq): Boolean {
    if(a.atoms.count() == b.atoms.count())
      return a.atoms.indices.all { v: Int -> is_equal(a.atoms[v], b.atoms[v]) }
    else
      return false
}

// TODO Implement!
private fun compare_maps(a: MalMap, b: MalMap) = a == b

private fun pr_str_core(seq: MalSeq) =
    seq.atoms.map { as_str(it, readable=true) }.joinToString(" ")

private fun str_core(seq: MalSeq) =
    seq.atoms.map { as_str(it, readable=false) }.joinToString("")

private val eof = ""

object core {
    val ns : Map<MalSymbol, MalFunc> = mutableMapOf(
        // Basic number ops.
        malSym("+") to malFun("plus")  { int_ops_reducer(Int::plus,  it) },
        malSym("-") to malFun("minus") { int_ops_reducer(Int::minus, it) },
        malSym("*") to malFun("times") { int_ops_reducer(Int::times, it) },
        malSym("/") to malFun("div")   { int_ops_reducer(Int::div,   it) },

        // pr-str: calls `pr_str` on each argument with `print_readably` set to true, joins the results with " " and returns the new string.
        to_fun("pr-str") {
            MalString(pr_str_core(it))
        },
        // `str`: calls `pr_str` on each argument with `print_readably` set to false, concatenates the results together ("" separator), and returns the new string.
        to_fun("str") {
            MalString(str_core(it))
        },
        // prn:  calls `pr_str` on each argument with `print_readably` set to true, joins the results with " ", prints the string to the screen and then returns `nil`.
        to_fun("prn") {
            println(pr_str_core(it))
            MalNil()
        },
        // `println`:  calls `pr_str` on each argument with `print_readably` set to false, joins the results with " ", prints the string to the screen and then returns `nil`.
        to_fun("println") {
            println(str_core(it))
            MalNil()
        },

        // list: take the parameters and return them as a list.
        to_fun("list") { it }, // we always get a list at this point
        // list?: return true if the first parameter is a list, false otherwise.
        to_fun("list?") { MalBoolean(it[0] is MalList) },
        // empty?: treat the first parameter as a list and return true if the list is empty and false if it contains any elements.
        to_fun("empty?") { MalBoolean(it[0] is MalSeq && (it[0] as MalSeq).atoms.isEmpty()) },
        // count: treat the first parameter as a list and return the number of elements that it contains.
        to_fun("count") { MalNumber((it[0] as MalSeq).atoms.count()) },

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
            val atom = it[0] as MalCljAtom
            val fn   = it[1] as MalCallable
            // Pull out args if there are any.
            val args = it.atoms.slice(2 .. (if(it.size > 2) it.size - 1 else 1))
            // Call the function with atom value + any args.
            val res  = fn(malListOf(listOf(atom.value) + args))
            atom.value = res
            res
        },

        to_fun("cons") {
            val rest = if(it.size > 1) it[1] as MalSeq else emptyMalList()
            malListOf(listOf(it.head()) + rest.atoms)
        },
        to_fun("concat") {
            malListOf(it.atoms.flatMap { (it as MalSeq).atoms })
        },

        to_fun("nth") {
            val seq = it[0] as MalSeq
            val idx = it[1] as MalNumber
            seq[idx]
        },
        to_fun("first") {
            val v = it[0]
            if (v is MalNil) {
                MalNil()
            }
            else if (v is MalSeq) {
                if(v.size == 0) MalNil() else v.head()
            }
            else {
                throw MalCoreEx("Can't fall 'first' on " + pr_str(v))
            }
        },
        to_fun("rest") {
            val v = it[0]
            if (v is MalSeq) {
                v.tail()
            }
            else {
                throw MalCoreEx("Can't fall 'rest' on " + pr_str(v))
            }
        },

        to_fun("throw") {
            throw when(it.size) {
                0    -> MalUserEx(MalString("error raised anon"))
                else -> MalUserEx(it[0])
            }
        },

        to_fun("apply") {
            // The first argument is a function and the last argument
            // is list (or vector). The arguments between the function
            // and the last argument (if there are any) are
            // concatenated with the final argument to create the
            // arguments that are used to call the function.
            val fn     = it[0]     as MalCallable
            val argSeq = it.last() as MalSeq
            val args   = it.atoms.slice(1 .. (if(it.size > 2) it.size - 2 else 0))
            fn(malListOf(argSeq.atoms + args))
        },
        to_fun("map") {
            val fn   = it[0] as MalCallable
            val args = it[1] as MalSeq
            malListOf(args.atoms.map { fn(malListOf(it)) })
        },

        to_fun("nil?") {
            MalBoolean(it[0] is MalNil)
        },
        to_fun("true?") {
            MalBoolean(it[0] == MalBoolean(true))
        },
        to_fun("false?") {
            MalBoolean(it[0] == MalBoolean(false))
        },
        to_fun("symbol?") {
            MalBoolean(it[0] is MalSymbol)
        },

        to_fun("symbol") {
            MalSymbol((it[0] as MalString).str)
        },
        to_fun("keyword") {
            val kw = it[0]
            if (kw is MalKeyword) kw else MalKeyword((kw as MalString).str)
        },
        to_fun("keyword?") {
            MalBoolean(it[0] is MalKeyword)
        },
        to_fun("vector") {
            MalVector(it.atoms)
        },
        to_fun("vector?") {
            MalBoolean(it[0] is MalVector)
        },
        to_fun("hash-map") {
            make_map(it)
        },
        to_fun("map?") {
            MalBoolean(it[0] is MalMap)
        },
        to_fun("assoc") {
            val m = it[0] as MalMap
            MalMap(m.pairs + make_map(it.tail()).pairs)
        },
        to_fun("dissoc") {
            val m = it[0] as MalMap
            MalMap(m.pairs - it.tail().atoms.map { it as MalString })
        },
        to_fun("get") {
            val m = it[0] as MalMap
            m.pairs.getOrDefault(it[1] as MalString, MalNil())
        },
        to_fun("contains?") {
            val m = it[0] as MalMap
            MalBoolean(m.pairs.contains(it[1]))
        },
        to_fun("keys") {
            val m = it[0] as MalMap
            malListOf(m.pairs.keys.toList())
        },
        to_fun("vals") {
            val m = it[0] as MalMap
            malListOf(m.pairs.values.toList())
        },
        to_fun("sequential?") {
            MalBoolean(it[0] is MalSeq)
        },

        to_fun("readline") {
            print((it[0] as MalString).str)
            val line = readLine()
            if (line == null) MalNil() else MalString(line.trim())
        },

        to_fun("meta") {
            val f = it[0] as MalCallable
            f.meta
        },
        to_fun("with-meta") {
            val f = it[0] as MalCallable
            f.withMeta(it[1])
        }
    )
}
