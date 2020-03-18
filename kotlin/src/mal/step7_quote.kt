fun READ(s: String) = read_str(s)

fun eval_ast(ast: MalType, env: Env, depth: Int) : MalType {
    return when(ast) {
        is MalList   -> MalList(ast.atoms.map { EVAL(it, env, depth + 1) }.toList())
        is MalSymbol -> env.get(ast)
        else -> ast
    }
}

fun make_env(pairs: MalList, outer_env: Env, depth: Int) : Env {
    val new_env = Env(outer_env)
    for (idx in pairs.atoms.indices step 2) {
        val k = pairs.atoms[idx] as MalSymbol
        val v = pairs.atoms[idx + 1]
        new_env.set(k, EVAL(v, new_env, depth + 1))
    }
    return new_env
}

fun is_true(cond: MalType) =
    when (cond) {
        is MalNil -> false
        is MalBoolean -> cond.bool
        else -> true
    }

// First implement a helper function `is_pair` that returns true if the
// parameter is a non-empty list.
fun is_pair(p: MalType) = p is MalList && p.size > 0

fun quasiquote(ast: List<MalType>) = quasiquote(*ast.toTypedArray())

fun quasiquote(vararg ast: MalType): MalType {
    println("Quasiquoting: " + ast.map(::pr_str).joinToString(" "))
    // if `is_pair` of `ast` is false: return a new list containing:
    //  a symbol named "quote" and `ast`.
    if (ast[0] !is MalList || !is_pair(ast[0])) {
        // XXX Should the symbol and ast be joined?
        return malListOf(malSym("quote"), ast[0])
    }
    // We know ast[0] is now a list with stuff in it.
    val fst  = ast[0]
    val rest = ast.drop(1)
    // else if the first element of `ast` is a symbol named "unquote":
    // return the second element of `ast`.
    return if (fst == malSym("unquote")) {
        ast[1]
    }
    // if `is_pair` of the first element of `ast` is true and the first
    // element of first element of `ast` (`ast[0][0]`) is a symbol named
    // "splice-unquote": return a new list containing: a symbol named
    // "concat", the second element of first element of `ast`
    // (`ast[0][1]`), and the result of calling `quasiquote` with the
    // second through last element of `ast`.
    else if (is_pair(fst) && fst is MalList && fst.head() == malSym("splice-unquote")) {
        malListOf(listOf(malSym("concat"), fst[1]) + quasiquote(rest))
    }
    // otherwise: return a new list containing: a symbol named "cons", the
    // result of calling `quasiquote` on first element of `ast`
    // (`ast[0]`), and the result of calling `quasiquote` with the second
    // through last element of `ast`.
    else {
        val head = if (fst is MalList) fst.head() else fst
        val tail = if (fst is MalList) fst.tail().atoms else rest
        // XXX Assumes we'll only get a list i.e (qq (1 2 (3 4))) not (qq (1 2) 3 4)
        malListOf(malSym("cons"), quasiquote(head), quasiquote(tail))
    }
}

fun newqq(ast: MalType) : MalType {
    return if (ast is MalList && ast.size > 0) {
        val fst  = ast.head()
        val rest = ast.tail()
        if (fst == malSym("unquote"))
            rest.head()
        else if (fst is MalList && fst.head() == malSym("splice-unquote"))
            malListOf(malSym("concat"), fst.tail().head(), newqq(rest))
        else
            malListOf(malSym("cons"), newqq(fst), newqq(rest))
    }
    else {
        malListOf(malSym("quote"), ast)
    }
}

var eval_count = 0
fun EVAL(cur_ast: MalType, cur_env: Env, depth: Int) : MalType {
//    val say = { m: String -> println(" ".repeat(depth) + m) }
    // Only use n when recursing into EVAL
    val n = depth + 1
    // Allow modification of where in the ast is being evaluated.
    var ast = cur_ast
    // Allow modification of which env is pointed at while evaluating.
    var env = cur_env
    eval_loop@ while (true) {
        eval_count++
        // The depth check is low enough so as not to hit a terminal StackOverflow error.
        // But eval_count is just an arbitrary limit.
        if (depth > 1012 || eval_count > 654321) {
            throw Exception("Recursion/eval limit hit: depth ${depth} / evalled ${eval_count}!")
        }

        if (ast !is MalList) {
            return eval_ast(ast, env, depth)
        }
        else {
            if (ast.atoms.isEmpty()) {
                return ast
            }
            else {
//                say(pr_str(ast[0]))
                val next = ast.head()
                val rest = ast.tail()
                if(next is MalSymbol) {
                    when(next.sym) {
                        "def!" -> {
                            val v    = EVAL(rest[1], env, n)
                            val name = (rest[0] as MalSymbol)
                            if (v is MalUserFunc) {
                                v.fn.name = name.sym
                            }
                            return env.set(name, v)
                        }
                        "let*" -> {
                            // Set env (i.e. the local variable passed in as second parameter of EVAL) to the new let environment. 
                            env = make_env((rest[0] as MalList), env, depth)
                            //  Set ast (i.e. the local variable passed in as first parameter of EVAL) to be the second ast argument.
                            ast = ast[2]
                            continue@eval_loop // TCO
                        }
                        "do" -> {
                            // change the eval_ast call to evaluate all the parameters except for the last (2nd list element up to but not including last).
                            eval_ast(rest.butlast(), env, depth)
                            // Set ast to the last element of ast. Continue at the beginning of the loop (env stays unchanged).
                            ast = rest.last()
                            continue@eval_loop // TCO
                        }
                        "if" -> {
                            // the condition continues to be evaluated, however, rather than evaluating the true or false branch, ast is set to the unevaluated value of the chosen branch.
                            ast = if(is_true(EVAL(rest[0], env, n))) rest[1] else
                                  if(rest.atoms.count() == 3)        rest[2] else ast
                            continue@eval_loop // TCO
                        }
                        "fn*" -> {
                            // The return value from the fn* special form will now become an object/structure with attributes that allow the default invoke case of EVAL to do TCO on mal functions. Those attributes are:
                            val binds = rest[0] as MalList
                            val body  = rest[1]
                            val func  = malFun("funccall") {
                                EVAL(body, Env(env, binds, it), n)
                            }
                            return MalUserFunc(body, binds, env, func)
                        }
                        "quote" -> {
                            return rest.head()
                        }
                        // This is called from `EVAL` with the first `ast` argument (second list element) and then `ast` is set to the result and execution continues at the top of the loop (TCO).
                        "quasiquote" -> {
                            // TODO TCO
                            // XXX Support >1 args?
                            ast = newqq(rest[0])
                            continue@eval_loop // TCO
                        }
                    }
                }

                val op   = eval_ast(ast, env, depth) as MalList
                val func = op.head()
                val args = op.tail()

                if(func is MalUserFunc) {
                    //  set ast to the ast attribute of f.
                    // Generate a new environment using the env and params attributes of f as the outer and binds arguments and rest ast arguments (list elements 2 through the end) as the exprs argument. Set env to the new environment. Continue at the beginning of the loop.
                    ast = func.ast
                    val binds = func.params.atoms.mapIndexed { index, atom ->
                        val k = atom as MalSymbol
                        val v = args[index]
                        listOf(k,v)
                    }.flatten().let { MalList(it) }
                    env = make_env(binds, func.env, depth)
                    continue@eval_loop // TCO
                }
                else if(func is MalFunc) {
                    return func(args)
                }
                else {
                    return func
//                    throw Exception("Don't know what to do with " + func)
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
    repl_env.set(malSym("eval"), malFun("eval") {
        val res = eval_ast(it, repl_env, 0)
        when(res) {
            is MalList -> res.last()
            else       -> res
        }
    })
    repl_env.set(malSym("*ARGV*"), malListOf(args.map(::MalString)))

    rep("""(def! load-file (fn* [f] (eval (read-string (str "(do " (slurp f) ")")))))""")
    rep("(def! not (fn* [v] (if v false true)))")

    repl@ while(true) {
        print("user> ")

        try {
            val line = readLine() ?: continue@repl
            if (setOf("quit","exit").contains(line.trim())) {
                println("Bye!")
                break@repl
            }
            println(rep(line))
        }
        catch(e: Exception) {
            println("Oh dear:" + e.toString())
            e.printStackTrace()
            eval_count = 0
        }
    }
}

