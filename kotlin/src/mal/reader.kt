// For each match captured within the parenthesis starting at char 6
// of the regular expression a new token will be created.
var tokenizer = Regex("""
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
  "(?:.|[^"])*"? |
  # Captures any sequence of characters starting with ; (tokenized).
  ;.* |
  # Captures a sequence of zero or more non special characters (e.g. symbols,
  # numbers, "true", "false", and "nil") and is sort of the inverse of the one
  # above that captures special characters (tokenized).
  [^\s\[\]{}('"`,;)]*
)
""",
RegexOption.COMMENTS
)

// This function will take a single string and return an array/list of all the tokens (strings) in it.
fun tokenize(s: String) : List<String> {
    // For some reason this fails where findAll doesn't.
    // if (!tokenizer.matches(s)) {
    //     throw Exception("Failed tokenizing")
    // }
    return tokenizer.findAll(s)
            .map { it.value.trim() }
            .filter { it.length > 0 }
            .toList()
}

// This object will store the tokens and a position.
class Reader(val tokens: List<String>) {
    var pos = 0
    // returns the token at the current position and increments the position
    fun next() : String {
        val s = tokens[pos]
        pos += 1
        return s
    }

    // just returns the token at the current position.
    fun peek() = tokens[pos]
}

var readLimit = 0

// This function will peek at the first token in the Reader object and
// switch on the first character of that token. If the character is a
// left paren then read_list is called with the Reader
// object. Otherwise, read_atom is called with the Reader Object. The
// return value from read_form is a mal data type.
fun read_form(r: Reader) : MalType {
    try {
        return when(r.peek()) {
            "("  -> read_list(r)
            else -> read_atom(r)
        }
    }
    catch(e: IndexOutOfBoundsException) {
        throw Exception("Ran out of tokens, missing right paren?")
    }
    finally {
        readLimit = 0
    }
}

fun read_list(r: Reader) : MalList {
//    println("Reading list: " + r)
    r.next() // Move past the opening paren.
    val list : MutableList<MalType> = mutableListOf()
    while(r.peek() != ")") {
        list.add(read_form(r))
        // Safety limit to prevent the REPL never coming back.
        readLimit += 1
        if (readLimit > 200) {
            throw Exception("Parser found no end :(")
        }
    }
    return MalList(list)
}

fun is_number(s: String) = Regex("\\d+").matches(s)

// This function will look at the contents of the token and return the
// appropriate scalar (simple/single) data type value.
fun read_atom(r: Reader) : MalAtom {
//    println("Reading atom: " + r)
    val t = r.next()
    return if (is_number(t)) {
        MalNumber(t.toInt())
    }
    else {
        MalSymbol(t)
    }
}

// This function will call tokenize and then create a new Reader
// object instance with the tokens. Then it will call read_form with
// the Reader instance.
fun read_str(s: String) = read_form(Reader(tokenize(s)))
