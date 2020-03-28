// For each match captured within the parenthesis starting at char 6
// of the regular expression a new token will be created.

// TODO Support escaped double quotes e.g "foo \"bar\" baz" -> """foo "bar" baz"""
private var tokenizer = Regex("""
# Matches any number of whitespaces or commas
[\s,]*
(?:
  # Captures the special two-characters ~@ (tokenized).
  ~@ |
  # Captures any special single character, one of []{}()'`~^@ (tokenized).
  [\[\]{}()'`~^@] |
  # Starts capturing at a double-quote and stops at the next double-quote unless
  # it was preceded by a backslash in which case it includes it until the next
  # double-quote (tokenized). It will also match unbalanced strings (no ending
  # double-quote) which should be reported as an error.
  "(?:\\"|[^"])*?" |
  # Captures any sequence of characters starting with ; (tokenized).
  ;.* |
  # Captures a sequence of zero or more non special characters (e.g. symbols,
  # numbers, "true", "false", and "nil") and is sort of the inverse of the one
  # above that captures special characters (tokenized).
  [^\s\[\]{}('"`,;)]*
)
""",
setOf(RegexOption.COMMENTS, RegexOption.MULTILINE)
)

// This function will take a single string and return an array/list of all the tokens (strings) in it.
fun tokenize(s: String) : List<String> {
    // For some reason this fails where findAll doesn't.
    // if (!tokenizer.matches(s)) {
    //     throw MalCoreEx("Failed tokenizing")
    // }
    return tokenizer.findAll(s)
            .map    { it.value.trim() }
            .filter { it.length > 0 }
            .filter { !it.startsWith(";") }
            .toList()
}

// This object will store the tokens and a position.
class Reader(val tokens: List<String>) {
    var pos = 0
    // returns the token at the current position and increments the position
    fun next() = tokens[pos++]
    // Check whether we're at the end.
    fun isLast() = pos == (tokens.size - 1)
    // just returns the token at the current position.
    fun peek() = tokens[pos]
}

private fun is_number(s: String) = Regex("\\d+").matches(s)

private fun is_bool(s: String) = setOf("true", "false").contains(s)

private fun make_atom_deref(token: String) = MalList(listOf("deref", token).map(::malSym))

// Reflect any changes in printer.kt
private val readEscapeMap = mapOf(
    "\\\"" to "\"",
    "\\n"  to "\n",
    "\\\\" to "\\"
)
// Bleurgh, the escapes need escapes as they become interpolated into Regex ;_;
// So we need to manage three levels escaping different >_<
private val readEscapes = Regex(listOf("\\\\\"", "\\\\n", "\\\\\\\\").joinToString("|", "(", ")"))

private fun make_string(s: String) =
    MalString(s.replace(readEscapes) { readEscapeMap.get(it.value) ?: it.value })

private fun make_with_meta(r: Reader, n: Int): MalType {
    val meta = read_form(r, n)
    val func = read_form(r, n)
    return malListOf(malSym("with-meta"), func, meta)
}

private fun read_atom(r: Reader, n: Int) : MalType {
    //    println("Reading atom: " + r)
    val t = r.next()
    return when {
        t[0] == '"'  -> make_string(t.substring(1 .. t.length - 2))
        t[0] == ':'  -> MalKeyword(t.substring(1 .. t.length - 1))
        t[0] == '^'  -> make_with_meta(r, n)
        is_number(t) -> MalNumber(t.toInt())
        is_bool(t)   -> MalBoolean(t == "true")
        t == "nil"   -> MalNil()
        t == "@"     -> make_atom_deref(r.next())
        t == "'"     -> malListOf(malSym("quote"), read_form(r, n))
        t == "`"     -> malListOf(malSym("quasiquote"), read_form(r, n))
        t == "~"     -> malListOf(malSym("unquote"), read_form(r, n))
        t == "~@"    -> malListOf(malSym("splice-unquote"), read_form(r, n))
        else         -> MalSymbol(t)
    }
}

private fun make_map(pairs: List<MalType>) : MalMap {
    if(pairs.size % 2 != 0)
        throw MalUserEx(MalString("maps requires an even number of items, got ${pairs.size} items"))

    val map : MutableMap<MalKey, MalType> = mutableMapOf()
    for (idx in pairs.indices step 2) {
        val k = pairs[idx] as MalKey
        val v = pairs[idx + 1]
        map[k] = v
    }
    return MalMap(map)
}

fun make_map(pairs: MalSeq) = make_map(pairs.atoms)

private var readLimit = 0

// Safety limit to prevent the REPL never coming back.
private fun check_limit() {
    readLimit++
    if (readLimit > 1024) {
        throw MalCoreEx("Parser found no end :/")
    }
}

// This function will peek at the first token in the Reader object and
// switch on the first character of that token. If the character is a
// left paren then read_list is called with the Reader
// object. Otherwise, read_atom is called with the Reader Object. The
// return value from read_form is a mal data type.
fun read_form(r: Reader, n: Int) : MalType {
//    println("v1> " + " ".repeat(n) + "read_form")
    try {
        return when(r.peek()) {
            "("  -> MalList(read_seq(")", r, n + 1))
            "["  -> MalVector(read_seq("]", r, n + 1))
            "{"  -> make_map(read_seq("}", r, n + 1))
            else -> read_atom(r, n + 1)
        }
    }
    catch(e: IndexOutOfBoundsException) {
        throw MalCoreEx("Ran out of tokens, missing right paren?")
    }
}

private fun read_seq(endTok: String, r: Reader, n: Int) : List<MalType> {
    r.next() // Move past the opening paren.
//    val say = { m: String -> println("v1> " + " ".repeat(n) + m) }
    val list : MutableList<MalType> = mutableListOf()
    while(r.peek() != endTok) {
//        say("at token: " + r.peek())
        list.add(read_form(r, n))
        check_limit()
    }
    if(!r.isLast()) r.next()
//    say("returning list!")
    return list
}

private fun read_form_safely(r: Reader) : MalType {
    try {
        return if(r.tokens.isEmpty()) {
            emptyMalList()
        }
        else {
            read_form(r, 0)
        }
    }
    finally {
        readLimit = 0
    }
}

// This function will call tokenize and then create a new Reader
// object instance with the tokens. Then it will call read_form with
// the Reader instance.
fun read_str(s: String) = read_form_safely(Reader(tokenize(s)))
