import arrow.analysis.*

// (1) class invariants
// but no parametrized refinements

class Pos(val value: Int) {
    init {
        require(value > 0)
    }
}

fun invariant() {
    val v = readLine()?.toInt()!!
    // fails compilation
//    val p1 = Pos(v)
    if (v > 0) {
        val p2 = Pos(v)
    }
}

// (2) pre- and post-conditions
// are actually preferred to class invariants

fun increment(x: Int): Int {
    pre (x >= 0) {"x is non-negative"}
    return (x + 1).post({it > 0}) {"result is positive"}
}

fun conditions() {
    val v = readLine()?.toInt()!!
    if (v > 0) {
        // implicit refinement (satisfiability of pre-condition)
        val p1 = increment(v)
        // refinement is carried through calls
        val p2 = increment(p1)
    }
}

// (3) loops invariants
fun loop() {
    val list = listOf(1, 2, 3)
    var evens = 0.invariant({it >= 0}) {"number of even elements"}
    for (x in list) {
        if (x % 2 == 0) {
            evens = evens + 1
        }
    }

    // allowed to pass to `increment`
    val result = increment(evens)
}

// (4) dependent typing
fun <T> find(list: List<T>, elem: T): Int {
    var i = 0.invariant(
        {it >= 0 && it < list.size}
    ) { "index of element" }
    while (true) {
        if (list[i] == elem) {
            return i.post(
                // dependent refinement!
                {it >= -1 && it < list.size}
            ) {"index of element"}
        }
        if (i + 1 == list.size) {
            return -1
        }
        i = i + 1
    }
}

fun dependent() {
    val list = listOf(1, 2, 3)
    val elem = 2
    val i = find(list, elem)
    // pre-condition of `increment` is satisfied!
    val p = increment(list.size - i)
}