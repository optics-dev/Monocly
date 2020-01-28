package optics.poly

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.language.implicitConversions
import optics.poly.Optional

class ErrorCompositionTest extends AnyFunSuite with Matchers {

  test("compose with different errors") {
    val some3 = EPPrism.some("err") >>> EPPrism.some(2) >>> EPPrism.some[Boolean, Int, String](true)

    some3.getOrError(Some(Some(Some(1)))) shouldEqual Right(1)
    some3.getOrError(Some(Some(None))) shouldEqual Left(true)
    some3.getOrError(Some(None)) shouldEqual Left(2)
    some3.getOrError(None) shouldEqual Left("err")
  }

  test("traversal errors level 1") {
    EPTraversal.list.toListOrError(List(1, 2)) shouldEqual Right(List(1, 2))
    EPTraversal.list.toListOrError(Nil) shouldEqual Left("List is empty")
  }

  test("traversal errors level 2") {
    (EPTraversal.list >>> PPrism.some).toListOrError(List(Some(1), Some(2))) shouldEqual Right(List(1, 2))
    // TODO should be Right(List(2))
    (EPTraversal.list >>> PPrism.some).toListOrError(List(None, Some(2))) shouldEqual Left("None is not a Some")
  }

}
