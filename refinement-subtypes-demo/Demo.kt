// (1) refinement subtypes

refinement Pos = Int satisfies { it > 0 }
refinement EvenPos = Pos satisfies { it % 2 == 0 }

// (2) subtyping

fun useInt(i: Int) = println("Using Int: $i")
fun usePos(p: Pos) = println("Using Pos: $p")

fun subtyping(i: Int, ep: EvenPos) {
    useInt(ep) // OK
//    usePos(i) // FAIL: Pos expected
    usePos(i as Pos) // OK, RUNTIME CHECK
}

// (3) type operations

fun typeOps(i: Int) {
    val refined = i as? EvenPos
    refined?.let { println("EvenPos: $it") } ?: println("Not EvenPos")

    try {
        val refined = i as Pos
        println("EvenPos: $refined")
    } catch (e: TypeCastException) {
        println("Not EvenPos")
    }

    // smartcasts
    if (i is EvenPos) {
        usePos(i) // smarcasted here
    } else {
        println("Not EvenPos")
    }
}

// (4) operations

fun Int.testOp(): String = "Int.testOp"
@JvmName("posTestOp") // because of poor implementation
fun Pos.testOp(): String = "Pos.testOp"

// but this does not work!
infix operator fun EvenPos.plus(other: EvenPos): EvenPos = TODO()

fun operations(ep: EvenPos) {
    println("testOp result: ${ep.testOp()}")
    val double = ep + ep
    //usePos(double) // FAIL: Int expected
    println("double: $double")
}

fun main() {
    subtyping(42, 36 as EvenPos)
    typeOps(42)
    operations(24 as EvenPos)
}