// Use named variables otherwise we have to escape escaping x_x
private val q  = "\u0022" // '"'  U+0022 &quot; QUOTATION MARK (Other_Punctuation)
private val bs = "\u005C" // '\'  U+005C &bsol; REVERSE SOLIDUS (Other_Punctuation)

// Reflect any changes in reader.kt
private val printEscapeMap = mapOf(
    "$q"  to "$bs$q",  // " to \"
    "\n"  to "${bs}n", // ␤ to \n
    "$bs" to "$bs$bs"  // \ to \\
)
// Bleurgh, the escapes need escapes as they become interpolated into Regex ;_;
// So we need to manage three levels escaping different >_<
private val printEscapes =
    Regex(listOf(q, "${bs}n", "$bs$bs(?![n$q])").joinToString("|", "(", ")"))

private fun malstr_as_string(s: String) =
    q + s.replace(printEscapes) { printEscapeMap.get(it.value) ?: it.value } + q

// … does the opposite of read_str: take a mal data structure and
// return a string representation of it.
fun pr_str(v: MalType, print_readably: Boolean = true) : String {
    // Close over print_readably to simplify code below.
    val pr     = { w: MalType -> pr_str(w, print_readably) }
    val pr_map = { p: Map.Entry<MalKey, MalType> -> pr(p.key) + " " + pr(p.value) }

    // Inner function to save repeating 'print_readably' in every call.
    return when(v) {
        is MalList     -> v.atoms.map(pr).joinToString(" ", "(", ")")
        is MalVector   -> v.atoms.map(pr).joinToString(" ", "[", "]")
        is MalMap      -> v.pairs.map(pr_map).joinToString(" ", "{", "}")
        is MalNumber   -> v.num.toString()
        is MalKeyword  -> ":${v.kw}"
        is MalString   -> if(print_readably) malstr_as_string(v.str) else v.str
        is MalSymbol   -> v.sym
        is MalBoolean  -> v.bool.toString()
        is MalCljAtom  -> "(atom ${pr(v.value)})"
        is MalNil      -> "nil"
        is MalCallable -> "#<${v.name}>"
        is MalUserEx   -> "Exception raised: ${pr(v.src)}"
        else -> "Can't stringify a "+v::class
    }
}
