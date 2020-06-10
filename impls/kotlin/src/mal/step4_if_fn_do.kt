fun READ(s: String) = read_str(s)

fun eval_ast(ast: MalType, env: Env) : MalType {
    return when(ast) {
        is MalList   -> MalList(ast.atoms.map { EVAL(it, env) }.toList())
        is MalVector -> MalVector(ast.atoms.map { EVAL(it, env) }.toList())
        is MalMap    -> malMapOf(ast.pairs.map { (k,v) -> k to EVAL(v, env) })
        is MalSymbol -> env.get(ast)
        else -> ast
    }
}

fun make_env(pairs: MalSeq, outer_env: Env) : Env {
    val new_env = Env(outer_env)
    for (idx in pairs.atoms.indices step 2) {
        val k = pairs.atoms[idx] as MalSymbol
        val v = pairs.atoms[idx + 1]
        new_env.set(k, EVAL(v, new_env))
    }
    return new_env
}

fun is_true(cond : MalType) =
    when(cond) {
        is MalNil -> false
        is MalBoolean -> cond.bool
        else -> true
    }

fun EVAL(ast: MalType, env: Env) : MalType {
    if (ast !is MalList) {
        return eval_ast(ast, env)
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
                        val v = EVAL(args[1], env)
                        return env.set((args[0] as MalSymbol), v)
                    }
                    "let*" -> return EVAL(args[1], make_env((args[0] as MalSeq), env))
                    "do" -> return (eval_ast(args, env) as MalList).last()
                    "if" -> {
                        val body = if(is_true(EVAL(args[0], env))) args[1] else
                                   if(args.atoms.count() == 3)     args[2] else MalNil()
                        return EVAL(body, env)
                    }
                    "fn*" -> {
                        val binds = args[0] as MalSeq
                        val body  = args[1]
                        return malFun("funccall") { EVAL(body, Env(env, binds, it)) }
                    }
                }
            }
            val l = eval_ast(ast, env) as MalList
            val f = l.head() as MalFunc
            return f(l.tail())
        }
    }
}

fun PRINT(v: MalType) = pr_str(v)

fun rep(s: String) = PRINT(EVAL(READ(s), repl_env))

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
        }
    }
}

