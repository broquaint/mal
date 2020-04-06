fun READ(s: String) = read_str(s)

val repl_env : Map<String, MalFunc> = mapOf(
        "+" to malFun("+") { MalNumber((it[0] as MalNumber).num + (it[1] as MalNumber).num) },
        "-" to malFun("-") { MalNumber((it[0] as MalNumber).num - (it[1] as MalNumber).num) },
        "*" to malFun("*") { MalNumber((it[0] as MalNumber).num * (it[1] as MalNumber).num) },
        "/" to malFun("/") { MalNumber((it[0] as MalNumber).num / (it[1] as MalNumber).num) }
)

// Create a new function eval_ast which takes ast (mal data type) and
// an associative structure (the environment from above). eval_ast
// switches on the type of ast as follows:
// 
//  * symbol: lookup the symbol in the environment structure and return the value or raise an error if no value is found
//  * list: return a new list that is the result of calling EVAL on each of the members of the list
//  * otherwise just return the original ast value

fun eval_ast(ast: MalType, env: Map<String, MalFunc>, depth: Int) : MalType {
//    print("eval_ast: ".repeat(depth))
//    println(PRINT(ast))
    return when(ast) {
        is MalList   -> MalList(ast.atoms.map { EVAL(it, env, depth) }.toList())
        is MalVector -> MalVector(ast.atoms.map { EVAL(it, env, depth + 1) }.toList())
        is MalMap    -> malMapOf(ast.pairs.map { (k,v) -> k to EVAL(v, env, depth + 1) })
        is MalSymbol -> env[ast.sym] ?: throw Exception("Unknown symbol '${ast.sym}'")
        else -> ast
    }
}

// Modify EVAL to check if the first parameter ast is a list.
// * ast is not a list: then return the result of calling eval_ast on it.
// * ast is a empty list: return ast unchanged.
// * ast is a list: call eval_ast to get a new evaluated list. Take the first
//   item of the evaluated list and call it as function using the rest of the
//   evaluated list as its arguments.

var eval_count = 0
fun EVAL(ast: MalType, env: Map<String, MalFunc>, depth: Int) : MalType {
//    print("EVAL____: ".repeat(depth))
//    println(PRINT(ast))

    eval_count += 1
    if (depth > 200 || eval_count > 500) {
        throw Exception("Recursion fail :(")
    }

    if (ast !is MalList) {
        return eval_ast(ast, env, depth)
    }
    else {
        if (ast.atoms.isEmpty()) {
            return ast
        }
        else {
            val l = eval_ast(ast, env, depth + 1)
            val f = ((l as MalList).head() as MalFunc)
            return f(l.tail())
        }
    }
}

fun PRINT(v: MalType) = pr_str(v)

fun rep(s: String) {
    println(PRINT(EVAL(READ(s), repl_env, 0)))
}

fun main(args: Array<String>) {
    while(true) {
        print("user> ")

        try {
            readLine()?.let { rep(it) }
        }
        catch(e: Exception) {
            println("Oh dear:" + e.toString())
        }
    }
}


