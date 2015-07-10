package ohnosequences.test

import org.scalatest.FunSuite

import ohnosequences.obeli._
import ohnosequences.monoid._

class ObeliTest extends FunSuite {

  case class right[X, Y](x: X, y: Y) extends Arrow[X, Y](x, y) {
    type Obelus = left[Y, X]
    lazy val obelus = left(y, x)
  }

  case class left[Y, X](y: Y, x: X) extends Arrow[Y, X](y, x) {
    type Obelus = right[X, Y]
    lazy val obelus = right(x, y)
  }


  test("Dummy test coming from the template") {

    >>(right(1, 'a'), left('b', 2)).obelus

  }
}
