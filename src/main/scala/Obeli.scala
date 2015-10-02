package ohnosequences

object obeli {

  trait AnyArrow { arr =>

    type In; val in: In
    type Out; val out: Out

    // type Self <: In --> Out //>: arr.type <: AnyArrow
    // type Self >: arr.type <: AnyArrow
    type Self   <: AnyArrow.of[In, Out, Obelus] //Of[arr.type]]
    type Obelus <: AnyArrow.of[Out, In, Self]   //Of[arr.type]]
    // val  obelus: Obelus
  }

  // type SelfOf[A <: AnyArrow] = A#Self with AnyArrow { type In = A#In; type Out = A#Out }
  // type ObelusOf[A <: AnyArrow] = A#Obelus with AnyArrow { type In = A#Out; type Out = A#In }

  object AnyArrow {

    type from[I] = AnyArrow { type In = I }
    type to[O]   = AnyArrow { type Out = O }

    type of[A, B, O] = AnyArrow { type In = A; type Out = B; type Obelus = O }
  }

  type -->[I, O] = AnyArrow { type In = I; type Out = O }
  type ~~>[I, O] = AnyArrow { type In <: I; type Out <: O }

  abstract class Arrow[I, O](val in: I, val out: O) extends AnyArrow {
    type In = I
    type Out = O
  }

  case class id[X](x: X) extends Arrow[X, X](x, x) {
    type Self = this.type
    type Obelus = this.type
    lazy val obelus = this: Obelus
  }


  case class Compose[
    A, B, C,
    // F <: A --> B,
    // G <: B --> C
    F <: AnyArrow.of[A, B, FO], FO <: AnyArrow.of[B, A, F],
    G <: AnyArrow.of[B, C, GO], GO <: AnyArrow.of[C, B, G]
  ](f: F, //fo: FO,
    g: G //, go: GO
  ) extends AnyArrow { // Arrow[A, C](f.in, g.out) {

    type In = A
    lazy val in = f.in

    type Out = C
    lazy val out = g.out

    // type Self   = Compose[A, B, C, F, G]
    // type Obelus = Compose[C, B, A, ObelusOf[G], ObelusOf[F]]
    type Self   = Compose[A, B, C, F, FO, G, GO]
    type Obelus = Compose[C, B, A, GO, G, FO, F]

    // lazy val obelus = ??? // Compose[C, B, A, GO, G, FO, F](go, g, fo, f)
  }

  type >>[
    F <: AnyArrow,
    G <: AnyArrow.from[F#Out]
  ] = Compose[F#In, F#Out, G#Out,
    // it doesn't see that F#Self#In = F#In, etc.
    F#Self, F#Obelus,
    G#Self, G#Obelus
  ]

  // implicit def arrowSyntax[A <: AnyArrow](a: A): ArrowSyntax[A] = ArrowSyntax[A](a)
  // case class ArrowSyntax[A <: AnyArrow](a: A) {
  //
  //   def >>[B <: AnyArrow.from[A#Out]](b: B): A >> B = Compose(a, b)
  // }

}

// object monoid {
//
//   import obeli._
//
//   // this is just a representation
//   case class ×[A, B](a: A, b: B)
//
//   case class Multiply[
//     FA, FB, F <: FA ~~> FB,
//     GA, GB, G <: GA ~~> GB
//   ](f: F, g: G) extends Arrow[
//     FA × GA,
//     FB × GB
//   ](×(f.in, g.in),
//     ×(f.out, g.out)
//   ) {
//
//     type     Obelus = Multiply[FB, FA, F#Obelus, GB, GA, G#Obelus]
//     lazy val obelus = Multiply[FB, FA, F#Obelus, GB, GA, G#Obelus](f.obelus, g.obelus)
//   }
//
//   type ⊗[F <: AnyArrow, G <: AnyArrow] = Multiply[
//     F#In, F#Out, F,
//     G#In, G#Out, G
//   ]
//
//   implicit def monoidSyntax[A <: AnyArrow](a: A): MonoidSyntax[A] = MonoidSyntax[A](a)
//   case class MonoidSyntax[A <: AnyArrow](a: A) {
//
//     def ⊗[B <: AnyArrow](b: B): A ⊗ B = Multiply(a, b)
//   }
//
// }
