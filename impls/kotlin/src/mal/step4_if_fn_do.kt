fun READ(s: String) = read_str(s)

// Create a new function eval_ast which takes ast (mal data type) and
// an associative structure (the environment from above). eval_ast
// switches on the type of ast as follows:
// 
//  * symbol: lookup the symbol in the environment structure and return the value or raise an error if no value is found
//  * list: return a new list that is the result of calling EVAL on each of the members of the list
//  * otherwise just return the original ast value

fun eval_ast(ast: MalType, env: Env, depth: Int) : MalType {
//    print("eval_ast: ".repeat(depth))
//    println(PRINT(ast))
    return when(ast) {
        is MalList   -> MalList(ast.atoms.map { EVAL(it, env, depth + 1) }.toList())
        is MalVector -> MalVector(ast.atoms.map { EVAL(it, env, depth + 1) }.toList())
        is MalMap    -> malMapOf(ast.pairs.map { (k,v) -> k to EVAL(v, env, depth + 1) })
        is MalSymbol -> env.get(ast)
        else -> ast
    }
}

/*

    do: Evaluate all the elements of the list using eval_ast and return the final evaluated element.
    if: Evaluate the first parameter (second element). If the result (condition) is anything other than nil or false, then evaluate the second parameter (third element of the list) and return the result. Otherwise, evaluate the third parameter (fourth element) and return the result. If condition is false and there is no third parameter, then just return nil.
    fn*: Return a new function closure. The body of that closure does the following:
        Create a new environment using env (closed over from outer scope) as the outer parameter, the first parameter (second list element of ast from the outer scope) as the binds parameter, and the parameters to the closure as the exprs parameter.
        Call EVAL on the second parameter (third list element of ast from outer scope), using the new environment. Use the result as the return value of the closure.

*/

fun make_env(pairs: MalSeq, outer_env: Env, depth: Int) : Env {
    val new_env = Env(outer_env)
    for (idx in pairs.atoms.indices step 2) {
        val k = pairs.atoms[idx] as MalSymbol
        val v = pairs.atoms[idx + 1]
        new_env.set(k, EVAL(v, new_env, depth + 1))
    }
    return new_env
}

fun is_true(cond : MalType) =
    when(cond) {
        is MalNil -> false
        is MalBoolean -> cond.bool
        else -> true
    }

var eval_count = 0
fun EVAL(ast: MalType, env: Env, depth: Int) : MalType {
//    print("EVAL____: ".repeat(depth))
//    println(PRINT(ast))

    // Only use n when recursing into EVAL
    val n = depth + 1
    eval_count += 1
    if (depth > 10000 || eval_count > 50000) {
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
            val op   = ast.head()
            val args = ast.tail()
            if(op is MalSymbol) {
                when(op.sym) {
                    "def!" -> {
                        val v = EVAL(args[1], env, n)
                        return env.set((args[0] as MalSymbol), v)
                    }
                    "let*" -> return EVAL(args[1], make_env((args[0] as MalSeq), env, depth), n)
                    "do" -> return (eval_ast(args, env, depth) as MalList).last()
                    "if" -> {
                        val body = if(is_true(EVAL(args[0], env, n))) args[1] else
                                   if(args.atoms.count() == 3)        args[2] else MalNil()
                        return EVAL(body, env, n)
                    }
                    "fn*" -> {
                        val binds = args[0] as MalSeq
                        val body  = args[1]
                        return malFun("funccall") { EVAL(body, Env(env, binds, it), n) }
                    }
                }
            }
            val l = eval_ast(ast, env, depth) as MalList
            val f = l.head() as MalFunc
            return f(l.tail())
        }
    }
}

fun PRINT(v: MalType) = pr_str(v)

fun rep(s: String) = PRINT(EVAL(READ(s), repl_env, 0))

val repl_env = Env().apply {
    core.ns.forEach { (k,v) -> set(k, v) }
}

fun main(args: Array<String>) {
    rep("(def! not (fn* [v] (if v false true)))")
    while(true) {
        print("user> ")

        try {
            readLine()?.let { println(rep(it)) }
        }
        catch(e: Exception) {
            println("Oh dear:" + e.toString())
            eval_count = 0
        }
    }
}

