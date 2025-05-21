package org.example

@JvmInline
@Refinement
value class Positive(val value: Int) {
    init {
        require(0 < value)
    }
}

@JvmInline
@Refinement
value class NoLessThan42(val value: Int) {
    init {
        require(value >= 42)
    }
}

@JvmInline
@Refinement
value class From0To128(val value: Int) {
    init {
        require(value >= 0 && value <= 128)
    }
}

@JvmInline
@Refinement
value class From0To64(val value: Int) {
    init {
        require((value >= 0 && value <= 20) || (value <= 64 && value > 5))
    }
}

//@JvmInline
//@Refinement
//value class Incorrect(val value: Int) {
//    init {
//        require((value >= 0 && value <= 20) || (value <= 64 && value > 32))
//    }
//}

@JvmInline
@Refinement
value class Empty(val value: Int) {
    init {
        require(value >= 0 && value <= -20)
    }
}


fun <T: Positive> f(): Int = TODO()

fun playground() {
    val s: String = "some string"
    println(s is Int && s > 0)
}

//fun test1() {
//    val v = readLine()?.toInt()!!
//    val s = "som string"
//    if (v > 0 && s.isNotEmpty()) {
//        val t = Positive(v)
//    }
//    if (v >= 42) {
//        val t = NoLessThan42(v)
//    }
//    if (v >= 0 && v <= 128) {
//        val t = From0To128(v)
//    }
//    if (v >= 0 && v <= 64) {
//        val t = From0To64(v)
//    }
//    val t = Empty(v)
//}

//        fun test2() {
//            val v1 = 0
//            val v2 = v1 + 42
//            val t = Positive(v2)
//        }

//        fun test3() {
//            val v1 = readLine()?.toInt()!!
//            val v2 = readLine()?.toInt()!!
//            val v3 = -42
//            if (v1 > 0 && v2 > 0) {
//                val t = Positive(v1 * v2 - v3)
//            }
//        }

//        fun test4() {
//            val v1 = readLine()?.toInt()!!
//            while (v1 < 0) {
//                val t = From0To64(v1)
//                break
//            }
//        }

//        fun test5() {
//            var v1 = readLine()?.toInt()!!
//            val v2 = readLine()?.toInt()!!
//            if (v2 > 0 && v1 > 0) {
//                v1 = v1 + v2 * 5
//                val t1 = Positive(v1)
//                val t2 = Positive(t1.value * 3)
//            }
//        }

//        fun test6() {
//            val sc1 = SomeClass(readLine()?.toInt()!!)
//            val sc2 = SomeClass(readLine()?.toInt()!!)
//            if (sc1.v > 0) {
//                val t = Positive(sc2.v) // Negative case
//            }
//        }

//        fun test7() {
//            var v = 0
//            repeat(10) {
//                v = v + 1
//                // Failing =(
//                val t = Positive(v)
//            }
//        }

//        fun test8() {
//            var v1 = 0
//            var v2 = readLine()?.toInt()!!
//            while (v2 > 0 && v2 < 100) {
//                v1 = v1 + 1
//                val t = Positive(v2)
//                v2 = v2 - 1
//                val t2 = Positive(v1)
//            }
//        }

//        fun test9() {
//            val v = readLine()?.toInt()!!
//            if (v > 0) {
//                if (v < 10) {
//                    val t = Positive(v)
//                }
//            }
//        }