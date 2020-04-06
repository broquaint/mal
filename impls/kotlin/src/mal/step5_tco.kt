fun READ(s: String) = read_str(s)

fun eval_ast(ast: MalType, env: Env, depth: Int) : MalType {
    return when(ast) {
        is MalList   -> MalList(ast.atoms.map { EVAL(it, env, depth + 1) }.toList())
        is MalVector -> MalVector(ast.atoms.map { EVAL(it, env, depth + 1) }.toList())
        is MalMap    -> malMapOf(ast.pairs.map { (k,v) -> k to EVAL(v, env, depth + 1) })
        is MalSymbol -> env.get(ast)
        else -> ast
    }
}

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
fun EVAL(cur_ast: MalType, cur_env: Env, depth: Int) : MalType {
    // Only use n when recursing into EVAL
    val n = depth + 1
    // Allow modification of where in the ast is being evaluated.
    var ast = cur_ast
    // Allow modification of which env is pointed at while evaluating.
    var env = cur_env
    eval_loop@ while(true) {
        eval_count++
        // The depth check is low enough so as not to hit a terminal StackOverflow error.
        // But eval_count is just an arbitrary limit.
        if (depth > 1021 || eval_count > 654321) {
            throw Exception("Recursion/eval limit hit: depth ${depth} / evalled ${eval_count}!")
        }

        if (ast !is MalList) {
            return eval_ast(ast, env, depth)
        }
        else {
            if (ast.atoms.isEmpty()) {
                return ast
            }

            val next = ast.head()
            val rest = ast.tail()
            if(next is MalSymbol) {
                when(next.sym) {
                    "def!" -> {
                        val v = EVAL(rest[1], env, n)
                        return env.set((rest[0] as MalSymbol), v)
                    }
                    "let*" -> {
                        // Set env (i.e. the local variable passed in as second parameter of EVAL) to the new let environment. 
                        env = make_env((rest[0] as MalSeq), env, depth)
                        //  Set ast (i.e. the local variable passed in as first parameter of EVAL) to be the second ast argument.
                        ast = ast[2]
                        continue@eval_loop // TCO
                    }
                    "do" -> {
                        // change the eval_ast call to evaluate all the parameters except for the last (2nd list element up to but not including last).
                        eval_ast(rest.butlast(), env, depth)
                        // Set ast to the last element of ast. Continue at the beginning of the loop (env stays unchanged).
                        ast = rest.tail()
                        continue@eval_loop // TCO
                    }
                    "if" -> {
                        // the condition continues to be evaluated, however, rather than evaluating the true or false branch, ast is set to the unevaluated value of the chosen branch.
                        ast = if(is_true(EVAL(rest[0], env, n))) rest[1] else
                              if(rest.atoms.count() == 3)        rest[2] else MalNil()
                        continue@eval_loop // TCO
                    }
                    "fn*" -> {
                        // The return value from the fn* special form will now become an object/structure with attributes that allow the default invoke case of EVAL to do TCO on mal functions. Those attributes are:
                        val binds = rest[0] as MalSeq
                        val body  = rest[1]
                        return MalUserFunc(body, binds, env, "anon", MalNil()) {
                            EVAL(body, Env(env, binds, it), n)
                        }
                    }
                    else -> {
                        val op   = eval_ast(ast, env, depth) as MalList
                        val func = op.head()
                        val args = op.tail()

                        //  (def! sum-to (fn* (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))))
                        when(func) {
                            is MalUserFunc -> {
                                //  set ast to the ast attribute of f.
                                ast = func.ast
                                env = Env(func.env, func.params, args)
                                continue@eval_loop // TCO
                            }
                            is MalFunc -> return func(args)
                            else -> throw Exception("Don't know what to do with " + func)
                        }
                    }
                }
            }
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
    rep("(def! sum-to (fn* (n) (if (= n 0) 0 (+ n (sum-to (- n 1))))))")

    while(true) {
        print("user> ")

        try {
            readLine()?.let { println(rep(it)) }
        }
        catch(e: StackOverflowError) {
            println("Hit stack overflow at ${e.stackTrace.size}, top 15 frames were:")
            println(e.stackTrace.take(15).map{ "\t"+it }.joinToString("\n"))
        }
        catch(e: Exception) {
            println("Oh dear:" + e.toString())
        }
        finally {
            eval_count = 0
        }
    }
}

