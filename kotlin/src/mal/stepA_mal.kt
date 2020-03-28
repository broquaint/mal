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

fun is_true(cond: MalType) =
    when (cond) {
        is MalNil -> false
        is MalBoolean -> cond.bool
        else -> true
    }

// Not implemented as safe-casing is gooder.
// fun is_pair(p: MalType) = p is MalList && p.size > 0

fun quasiquote(ast: MalType) : MalType {
    return if (ast is MalList && ast.size > 0) {
        val fst  = ast.head()
        val rest = ast.tail()
        if (fst == malSym("unquote"))
            rest.head()
        else if (fst is MalList && fst.head() == malSym("splice-unquote"))
            malListOf(malSym("concat"), fst.tail().head(), quasiquote(rest))
        else
            malListOf(malSym("cons"), quasiquote(fst), quasiquote(rest))
    }
    else {
        malListOf(malSym("quote"), ast)
    }
}

fun is_macro_call(ast: MalType, env: Env) : Boolean {
    if(ast is MalList && ast.size > 0 && ast[0] is MalSymbol) {
        val sym = ast[0] as MalSymbol
        val e   = env.find(sym) ?: return false
        val v   = e.get(sym)
        return v is MalCallable && v.isMacro
    }
    else {
        return false
    }
}

fun macroexpand(orig_ast: MalType, env: Env) : MalType {
    var ast = orig_ast
    while(is_macro_call(ast, env)) {
        val l = ast as MalList
        //  Inside the loop, the first element of the ast list (a symbol), is looked up in the environment to get the macro function
        val f = env.get(l.head() as MalSymbol) as MalUserFunc
        // This macro function is then called/applied with the rest of the ast elements (2nd through the last) as arguments.
        // The return value of the macro call becomes the new value of ast.
        ast = f(l.tail())
    }
    return ast
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
            throw MalCoreEx("Recursion/eval limit hit: depth ${depth} / evalled ${eval_count}!")
        }

        ast = macroexpand(ast, env)

        if (ast !is MalList) {
            return eval_ast(ast, env, depth)
        }
        else {
            if (ast.atoms.isEmpty()) {
                return ast
            }
            else {
                val next = ast.head()
                val rest = ast.tail()
                if(next is MalSymbol) {
                    when(next.sym) {
                        "def!" -> {
                            val v    = EVAL(rest[1], env, n)
                            val name = (rest[0] as MalSymbol)
                            if (v is MalUserFunc) {
                                v.name = name.sym
                            }
                            return env.set(name, v)
                        }
                        "defmacro!" -> {
                            val v = EVAL(rest[1], env, n)
                            if (v !is MalUserFunc) {
                                throw MalCoreEx("Can't defmacro! with a: "+pr_str(v))
                            }
                            else {
                                v.isMacro = true
                            }
                            val name = (rest[0] as MalSymbol)
                            return env.set(name, v)
                            
                        }
                        "macroexpand" -> {
                            return macroexpand(rest[0], env)
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
                            val binds = rest[0] as MalSeq
                            val body  = rest[1]
                            return MalUserFunc(body, binds, env, "anon", MalNil()) {
                                EVAL(body, Env(env, binds, it), n)
                            }
                        }
                        "quote" -> {
                            return rest.head()
                        }
                        // This is called from `EVAL` with the first `ast` argument (second list element) and then `ast` is set to the result and execution continues at the top of the loop (TCO).
                        "quasiquote" -> {
                            // TODO TCO
                            // XXX Support >1 args?
                            ast = quasiquote(rest[0])
                            continue@eval_loop // TCO
                        }
                        "try*" -> {
                            val body = rest[0]
                            return try {
                                EVAL(body, env, n)
                            }
                            catch(e: MalUserEx) {
                                if(rest.size > 1) {
                                    val catchSeq = rest[1] as MalSeq
                                    val catchSym = catchSeq[0]
                                    if (catchSym != malSym("catch*"))
                                    throw MalCoreEx("Expected 'catch*', got '${pr_str(catchSym)}'")
                                    val exBind    = catchSeq[1] as MalSymbol
                                    val catchBody = catchSeq[2]
                                    val catchEnv  = Env(env, malListOf(exBind), malListOf(e.src))
                                    EVAL(catchBody, catchEnv, n)
                                }
                                else {
                                    // Throw it up if nothing can catch it
                                    throw e
                                }
                            }
                        }
                    }
                }

                val op   = eval_ast(ast, env, depth) as MalList
                val func = op.head()
                val args = op.tail()

                if(func is MalUserFunc) {
                    ast = func.ast
                    env = Env(func.env, func.params, args)
                    continue@eval_loop // TCO
                }
                else if(func is MalFunc) {
                    return func(args)
                }
                else {
                    return func
//                    throw MalCoreEx("Don't know what to do with " + func)
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
    repl_env.set(malSym("*host-language*"), MalString("Kotlin"))

    rep("""(do
(def! *gensym-counter* (atom 0))

(def! gensym
 (fn* []
  (symbol (str "G__" (swap! *gensym-counter* (fn* [x] (+ 1 x)))))))

(defmacro! or
 (fn* (& xs)
  (if (empty? xs)
   nil
   (if (= 1 (count xs))
    (first xs)
    (let* (condvar (gensym))
     `(let* (~condvar ~(first xs))
       (if ~condvar ~condvar (or ~@(rest xs)))))))))

(def! load-file
 (fn* [f]
  (eval (read-string (str "(do " (slurp f) ")")))))

(defmacro! cond
 (fn* (& xs)
  (if (> (count xs) 0)
   (list
    'if (first xs)
     (if (> (count xs) 1)
      (nth xs 1)
      (throw "odd number of forms to cond"))
     (cons 'cond (rest (rest xs)))))))

(def! not
 (fn* [v]
  (if v false true)))
)""")
    rep("")

    if(args.size > 0) {
        repl_env.set(malSym("*ARGV*"), malListOf(args.drop(1).map(::MalString)))
        rep("""(load-file "${args[0]}")""")
    }
    else {
        repl_env.set(malSym("*ARGV*"), emptyMalList())
        rep("""(println (str "Mal [" *host-language* "]"))""")
        repl@ while(true) {
            print("user> ")

            try {
                val line = readLine() ?: continue@repl
                if (setOf("quit", "exit").contains(line.trim())) {
                    println("Bye!")
                    break@repl
                }
                println(rep(line))
            }
            catch(e: Throwable) {
                println(
                    when(e) {
                        is MalUserEx -> "Exception raised: " + pr_str(e.src)
                        is MalCoreEx -> "Error encountered: " + e.msg
                        else -> "Non mal exception: " + e.message
                    }
                )
                e.printStackTrace()
            }
            finally {
                eval_count = 0
            }
        }
    }
}

