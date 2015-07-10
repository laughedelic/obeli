package ohnosequences

object obeli {

  trait AnyArrow {

    type In
    type Out

    type Obelus <: Out --> In
    val  obelus: Obelus
  }

  object AnyArrow {

    type from[I] = AnyArrow { type In = I }
    type to[O]   = AnyArrow { type Out = O }
  }

  type -->[I, O] = AnyArrow { type In = I; type Out = O }
  type ~~>[I, O] = AnyArrow { type In <: I; type Out <: O }


  case class Compose[
    A, B, C,
    F <: A ~~> B,
    G <: B ~~> C
  ](f: F, g: G) extends AnyArrow {

    type In = A
    type Out = C

    type Obelus = Compose[C, B, A, G#Obelus, F#Obelus]
    val  obelus = Compose[C, B, A, G#Obelus, F#Obelus](g.obelus, f.obelus)
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
