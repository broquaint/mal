// … does the opposite of read_str: take a mal data structure and
// return a string representation of it.
fun pr_str(v: MalType) : String {
    return when(v) {
        is MalList   -> {
            "(" + v.atoms.map { pr_str(it) }.joinToString(" ") + ")"
        }
        is MalNumber -> v.num.toString()
        is MalSymbol -> v.sym
        is MalFunc   -> "MalFunc(...)"
        else -> ""
    }
}
