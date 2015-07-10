package ohnosequences

object obeli {

  trait AnyArrow {

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
  ](f: F, g: G) extends AnyArrow {

    type In = A
    lazy val in = f.in

    type Out = C
    lazy val out = g.out

    type     Obelus = Compose[C, B, A, G#Obelus, F#Obelus]
    lazy val obelus = Compose[C, B, A, G#Obelus, F#Obelus](g.obelus, f.obelus)
  }

  type >>[
    F <: AnyArrow,
    G <: AnyArrow.from[F#Out]
  ] = Compose[F#In, F#Out, G#Out, F, G]

  def >>[
    F <: AnyArrow,
    G <: AnyArrow.from[F#Out]
  ](f: F, g: G): F >> G = Compose(f, g)

}

object monoid {

  import obeli._

  case class ×[A, B](a: A, b: B)

  case class Multiply[
    FA, FB, F <: FA ~~> FB,
    GA, GB, G <: GA ~~> GB
  ](f: F, g: G) extends AnyArrow {

    type In = FA × GA
    lazy val in = ×(f.in, g.in): In

    type Out = FB × GB
    lazy val out = ×(f.out, g.out): Out

    type     Obelus = Multiply[FB, FA, F#Obelus, GB, GA, G#Obelus]
    lazy val obelus = Multiply[FB, FA, F#Obelus, GB, GA, G#Obelus](f.obelus, g.obelus)
  }

  type ⊗[F <: AnyArrow, G <: AnyArrow] = Multiply[F#In, F#Out, F, G#In, G#Out, G]

  def ⊗[F <: AnyArrow, G <: AnyArrow](f: F, g: G): F ⊗ G = Multiply(f, g)

}
