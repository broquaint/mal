// … does the opposite of read_str: take a mal data structure and
// return a string representation of it.
fun pr_str(v: MalType) : String {
    return when(v) {
        is MalList   -> {
            "(" + v.atoms.map { pr_str(it) }.joinToString(" ") + ")"
        }
        is MalVector   -> {
            "[" + v.atoms.map { pr_str(it) }.joinToString(" ") + "]"
        }
        is MalNumber   -> v.num.toString()
        is MalString   -> "\"${v.str}\"" // TODO Support escapes
        is MalSymbol   -> v.sym
        is MalBoolean  -> v.bool.toString()
        // Use this specific format to make tests pass :/
        is MalCljAtom  -> "(atom ${pr_str(v.value)})"
        is MalNil      -> "nil"
        is MalUserFunc -> "#<${v.name}>"
        is MalFunc     -> "#<${v.name}>"
        else -> ""
    }
}
