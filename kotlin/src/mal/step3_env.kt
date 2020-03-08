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
        is MalList   -> MalList(ast.atoms.map { EVAL(it, env, depth) }.toList())
        is MalSymbol -> env.get(ast)
        else -> ast
    }
}

// Modify EVAL to check if the first parameter ast is a list.
// * ast is not a list: then return the result of calling eval_ast on it.
// * ast is a empty list: return ast unchanged.
// * ast is a list: call eval_ast to get a new evaluated list. Take the first
//   item of the evaluated list and call it as function using the rest of the
//   evaluated list as its arguments.

/*
Modify the apply section of EVAL to switch on the first element of the
list:

    symbol "def!": call the set method of the current environment
(second parameter of EVAL called env) using the unevaluated first
parameter (second list element) as the symbol key and the evaluated
second parameter as the value.

    symbol "let*": create a new environment using the current
environment as the outer value and then use the first parameter as a
list of new bindings in the "let*" environment. Take the second
element of the binding list, call EVAL using the new "let*"
environment as the evaluation environment, then call set on the "let*"
environment using the first binding list element as the key and the
evaluated second element as the value. This is repeated for each
odd/even pair in the binding list. Note in particular, the bindings
earlier in the list can be referred to by later bindings. Finally, the
second parameter (third element) of the original let* form is
evaluated using the new "let*" environment and the result is returned
as the result of the let* (the new let environment is discarded upon
completion).

    otherwise: call eval_ast on the list and apply the first element to the rest as before.

*/

fun make_env(pairs: MalList, outer_env: Env, depth: Int) : Env {
    val new_env = Env(outer_env)
    for (idx in pairs.atoms.indices step 2) {
        val k = pairs.atoms[idx] as MalSymbol
        val v = pairs.atoms[idx + 1]
        new_env.set(k, EVAL(v, new_env, depth + 1))
    }
    return new_env
}

var eval_count = 0
fun EVAL(ast: MalType, env: Env, depth: Int) : MalType {
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
            val op   = ast.head()
            val args = ast.tail()
            if(op is MalSymbol) {
                when(op.sym) {
                    "def!" -> {
                        val v = EVAL(args[1], env, depth + 1)
                        return env.set((args[0] as MalSymbol), v)
                    }
                    "let*" -> return EVAL(args[1], make_env((args[0] as MalList), env, depth), depth + 1)
                }
            }
            val l = eval_ast(ast, env, depth + 1)
            val f = ((l as MalList).head() as MalFunc)
            return f(l.tail())
        }
    }
}

fun PRINT(v: MalType) = pr_str(v)

val repl_env = Env(null).apply {
    set(MalSymbol("+"), MalFunc({ a, b -> MalNumber((a as MalNumber).num + (b as MalNumber).num) }))
    set(MalSymbol("-"), MalFunc({ a, b -> MalNumber((a as MalNumber).num - (b as MalNumber).num) }))
    set(MalSymbol("*"), MalFunc({ a, b -> MalNumber((a as MalNumber).num * (b as MalNumber).num) }))
    set(MalSymbol("/"), MalFunc({ a, b -> MalNumber((a as MalNumber).num / (b as MalNumber).num) }))
}

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

