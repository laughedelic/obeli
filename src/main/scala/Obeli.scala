package ohnosequences

object obeli {

  trait AnyArrow { arr =>

    type In; val in: In
    type Out; val out: Out

    type Obelus <: Out --> In
    val  obelus: Obelus
  }

  object AnyArrow {

    type from[I] = AnyArrow { type In = I }
    type to[O]   = AnyArrow { type Out = O }
  }

  type -->[I, O] = AnyArrow { type In = I; type Out = O }
  type ~~>[I, O] = AnyArrow { type In <: I; type Out <: O }

  abstract class Arrow[I, O](val in: I, val out: O) extends AnyArrow {
    type In = I
    type Out = O
  }

  case class id[X](x: X) extends Arrow[X, X](x, x) {
    type Obelus = this.type
    lazy val obelus = this: Obelus
  }


  case class Compose[
    A, B, C,
    F <: A ~~> B,
    G <: B ~~> C
  ](f: F, g: G) extends Arrow[A, C](f.in, g.out) {

    type     Obelus = Compose[C, B, A, G#Obelus, F#Obelus]
    lazy val obelus = Compose[C, B, A, G#Obelus, F#Obelus](g.obelus, f.obelus)
  }

  type >>[
    F <: AnyArrow,
    G <: AnyArrow.from[F#Out]
  ] = Compose[F#In, F#Out, G#Out, F, G]

  implicit def arrowSyntax[A <: AnyArrow](a: A): ArrowSyntax[A] = ArrowSyntax[A](a)
  case class ArrowSyntax[A <: AnyArrow](a: A) {

    def >>[B <: AnyArrow.from[A#Out]](b: B): A >> B = Compose(a, b)
  }

}

object monoid {

  import obeli._

  // this is just a representation
  case class ×[A, B](a: A, b: B)

  case class Multiply[
    FA, FB, F <: FA ~~> FB,
    GA, GB, G <: GA ~~> GB
  ](f: F, g: G) extends Arrow[
    FA × GA,
    FB × GB
  ](×(f.in, g.in),
    ×(f.out, g.out)
  ) {

    type     Obelus = Multiply[FB, FA, F#Obelus, GB, GA, G#Obelus]
    lazy val obelus = Multiply[FB, FA, F#Obelus, GB, GA, G#Obelus](f.obelus, g.obelus)
  }

  type ⊗[F <: AnyArrow, G <: AnyArrow] = Multiply[
    F#In, F#Out, F,
    G#In, G#Out, G
  ]

  implicit def monoidSyntax[A <: AnyArrow](a: A): MonoidSyntax[A] = MonoidSyntax[A](a)
  case class MonoidSyntax[A <: AnyArrow](a: A) {

    def ⊗[B <: AnyArrow](b: B): A ⊗ B = Multiply(a, b)
  }

}
