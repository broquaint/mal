fun READ(s: String) = s

fun EVAL(s: String) = s

fun PRINT(s: String) = println(s)

fun rep(s: String) = PRINT(EVAL(READ(s)))

fun main(args: Array<String>) {
        while(true) {
                print("user> ")
                readLine()?.let { rep(it) }
        }
}
