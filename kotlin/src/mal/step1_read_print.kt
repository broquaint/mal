fun READ(s: String) = read_str(s)

fun EVAL(s: MalType) = s

fun PRINT(v: MalType) = pr_str(v)

fun rep(s: String) {
    println(PRINT(EVAL(READ(s))))
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

