package org.example

@Refinement
@JvmInline
value class Pos(val value: Int) {
    init {
        require(value >= 0)
    }
}

@Refinement
@JvmInline
value class UInt7(val value: Int) {
    init {
        require(value >= 0 && value <= 127)
    }
}

fun basics() {
    val v: Int = readLine()?.toInt()!!

    // FAILED_TO_DEDUCE_CORRECTNESS
    val p1: Pos = Pos(v)
    if (v > 42) {
        // DEDUCED_CORRECTNESS
        val p2: Pos = Pos(v)
    }
    if (v < -1) {
        // DEDUCED_INCORRECTNESS
        val p3: Pos = Pos(v)
    }

    when {
        v >= 5 && v <= 42 -> {
            // DEDUCED_CORRECTNESS
            val ui1: UInt7 = UInt7(v)
        }
    }

    if (v > 0) {
        // DEDUCED_CORRECTNESS
        val p4: Pos = Pos(2 * v + 42) // abstract evaluation
        // DEDUCED_CORRECTNESS
        val p5: Pos = Pos(p4.value + 1) // other refined values
    }

    var iter = 0
    while ((iter + 1) % 5 > 0) { // widening
        // DEDUCED_CORRECTNESS
        val p6: Pos = Pos(iter)
        iter += 1
    }

    var captured = 0
    repeat(42) { // does not work
        // FAILED_TO_DEDUCE_CORRECTNESS
        val p7: Pos = Pos(captured)
        captured += 1
    }
}

//@Refinement
//@JvmInline
//value class NEString(val value: String) {
//    init {
//        require(value.isNotEmpty())
//    }
//
//    infix operator fun plus(other: NEString): NEString =
//        NEString(this.value + other.value)
//}
//
//fun showcase(s1: NEString, s2: NEString) {
//    val s3: NEString = s1 + s2 + s1
//}



