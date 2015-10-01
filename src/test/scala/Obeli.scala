package ohnosequences.test

import org.scalatest.FunSuite

import ohnosequences.obeli._
import ohnosequences.monoid._

object context {

  case class right[X, Y](x: X, y: Y) extends Arrow[X, Y](x, y) {
    type Obelus = left[Y, X]
    lazy val obelus = left(y, x)
  }

  case class left[Y, X](y: Y, x: X) extends Arrow[Y, X](y, x) {
    type Obelus = right[X, Y]
    lazy val obelus = right(x, y)
  }

  def typed[T](t: T): T = t

}

class ObeliTest extends FunSuite {
  import context._

  test("Composing things") {

    val comp = right(1, 'a') >> left('b', 2)

    comp.obelus >> comp >> comp.obelus >> comp
  }

  test("multiplying things") {

    val mul = right(1, 'a') ⊗ right("foo", 'b')

    println((mul >> mul.obelus >> mul).out)
  }
}
