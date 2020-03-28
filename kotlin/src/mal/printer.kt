// Use named variables otherwise we have to escape escaping x_x
private val q  = "\u0022" // '"'  U+0022 &quot; QUOTATION MARK (Other_Punctuation)
private val bs = "\u005C" // '\'  U+005C &bsol; REVERSE SOLIDUS (Other_Punctuation)

// Reflect any changes in reader.kt
private val printEscapeMap = mapOf(
    "$q"  to "$bs$q",
    "\n"  to "${bs}n",
    "$bs" to "$bs$bs"
)
// Bleurgh, the escapes need escapes as they become interpolated into Regex ;_;
// So we need to manage three levels escaping different >_<
private val printEscapes = Regex(listOf(q, "${bs}n", "$bs$bs(?![n$q])").joinToString("|", "(", ")"))

private fun malstr_as_string(s: String) =
    s.replace(printEscapes) { printEscapeMap.get(it.value) ?: it.value }

// … does the opposite of read_str: take a mal data structure and
// return a string representation of it.
fun as_str(v: MalType, readable: Boolean) : String {
    return when(v) {
        is MalList -> {
            "(" + v.atoms.map { pr_str(it) }.joinToString(" ") + ")"
        }
        is MalVector -> {
            "[" + v.atoms.map { pr_str(it) }.joinToString(" ") + "]"
        }
        is MalMap -> {
            "{" + v.pairs.map { (k,v) -> pr_str(k) + " " + pr_str(v) }.joinToString(" ") + "}"
        }
        is MalNumber   -> v.num.toString()
        is MalKeyword  -> ":${v.kw}"
        is MalString   -> if(readable) malstr_as_string(v.str) else v.str
        is MalSymbol   -> v.sym
        is MalBoolean  -> v.bool.toString()
        // Use this specific format to make tests pass :/
        is MalCljAtom  -> "(atom ${pr_str(v.value)})"
        is MalNil      -> "nil"
        is MalUserFunc -> "#<${v.name}>"
        is MalFunc     -> "#<${v.name}>"
        is MalUserEx   -> "Exception raised: "+as_str(v, readable)
        else -> "Can't stringify a "+v::class
    }
}

// Only add quotes to strings at the point of output, otherwise functions like
// str end up with multiply quoted strings.
fun pr_str(v: MalType, readable: Boolean = true) : String {
    val s = as_str(v, readable)
    return if(v is MalString) "\"$s\"" else s
}
