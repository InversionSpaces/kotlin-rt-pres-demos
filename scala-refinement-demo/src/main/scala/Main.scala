import eu.timepit.refined.*
import eu.timepit.refined.collection.*
import eu.timepit.refined.api.*
import eu.timepit.refined.auto.*
import scala.collection.mutable.*

// (1) refinement types

case class Pos()

given Validate.Plain[Int, Pos] =
  Validate.fromPredicate(
    i => i > 0, // predicate
    i => s"$i is not a positive integer", // error message
    Pos()
  )

// Refined is an opaque type alias
val pos: Int Refined Pos = refineV[Pos](42).right.get

def useInt(x: Int) = println("Using integer: " + x)
def usePos(p: Int Refined Pos) = println("Using positive integer: " + p)

def subtyping(x: Int, p: Int Refined Pos): Unit = {
  // does not work out of the box
  // useInt(p)
  // usePos(x)
}

// (optional) intersection representation

sealed trait Tagged[P]

type IntersectRefined[T, P] = T & Tagged[P]

given RefType[IntersectRefined] with {
  override def unsafeWrap[T, P](t: T): IntersectRefined[T, P] =
    t.asInstanceOf[IntersectRefined[T, P]]

  override def unwrap[T, P](tp: IntersectRefined[T, P]): T = tp

  override def unsafeRewrap[T, A, B](
      ta: IntersectRefined[T, A]
  ): IntersectRefined[T, B] =
    ta.asInstanceOf[IntersectRefined[T, B]]
}

val taggedPos: IntersectRefined[Int, Pos] =
  RefType[IntersectRefined].refine(42).right.get

def taggedSubtyping(p: IntersectRefined[Int, Pos]): Unit = {
  useInt(p)
}

// (2) parametrized refinements

case class MultipleOf[N <: Int](n: N)

given [N <: Int](using ValueOf[N]): Validate.Plain[Int, MultipleOf[N]] =
  Validate.fromPredicate(
    i => i % valueOf[N] == 0,
    i => s"$i is not a multiple of ${valueOf[N]}",
    MultipleOf[N](valueOf[N])
  )

val multipleOf3: Int Refined MultipleOf[3] = refineV[MultipleOf[3]](9).right.get

// (3) inference rules

given [N <: Int, K <: Int](using
  ValueOf[N],
  ValueOf[K]
): Inference[MultipleOf[N], MultipleOf[K]] = Inference(
  isValid = valueOf[N] % valueOf[K] == 0,
  show = s"MultipleOf[${valueOf[N]}] ==> MultipleOf[${valueOf[K]}]"
)

def useMultipleOf3(m: Int Refined MultipleOf[3]): Unit = println(s"Using multiple of 3: $m")

def inference(): Unit = {
  val m9: Int Refined MultipleOf[9] = refineV[MultipleOf[9]](9).right.get
  useMultipleOf3(m9)
}

// a dirty hack, actually should work by macro
given multipleOfConversion [N <: Int, K <: Int](
  using i: Inference[MultipleOf[N], MultipleOf[K]]
): Conversion[Int Refined MultipleOf[N], Int Refined MultipleOf[K]] = {
  case v =>
    assert(i.isValid)
    v.asInstanceOf[Int Refined MultipleOf[K]]
}

// on mutability

def mutability() = {
  val list = ListBuffer(1, 2, 3)
  val refined = refineV[NonEmpty](list).right.get
  list.clear()
  println(s"Refined list after clear: $refined")
}

@main def main(): Unit = {
  println(s"Refined value: $pos")
  println(s"Tagged value: $taggedPos")

  println(s"Multiple of 3: $multipleOf3")

  inference()

  mutability()
}
