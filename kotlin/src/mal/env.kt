// Define an Env object that is instantiated with a single outer parameter and starts with an empty associative data structure property data.
class Env(val outer: Env? = null,
          val binds: MalSeq = emptyMalList(),
          val exprs: MalSeq = emptyMalList()) {

    val data : MutableMap<MalSymbol, MalType> = mutableMapOf()

    init {
        for(idx in binds.atoms.indices) {
            set(binds.atoms[idx] as MalSymbol, exprs.atoms[idx])
        }
    }

    // Define three methods for the Env object:
    // set: takes a symbol key and a mal value and adds to the data structure
    fun set(sym: MalSymbol, value: MalType): MalType {
        data.set(sym, value)
        return value
    }

    // find: takes a symbol key and if the current environment contains that key then return the environment. If no key is found and outer is not nil then call find (recurse) on the outer environment.
    fun find(sym: MalSymbol): Env? =
        if(data.contains(sym)) this else outer?.find(sym)

    // get: takes a symbol key and uses the find method to locate the environment with the key, then returns the matching value. If no key is found up the outer chain, then throws/raises a "not found" error.
    fun get(key: MalSymbol): MalType {
        val env = find(key) ?: throw Exception("Could not find '${key.sym}' in env")
        return env.data.getValue(key)
    }
}
