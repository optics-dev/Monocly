package optics.poly


import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.language.implicitConversions

class TraversalTest extends AnyFunSuite with Matchers {

  test("toListOrError") {
    NonEmptyPTraversal.pair.toListOrError((5, 6)) shouldEqual Right(List(5, 6))

    EPTraversal.list.toListOrError(List(1,2,3)) shouldEqual Right(List(1,2,3))
    EPTraversal.list.toListOrError(Nil) shouldEqual Left("List is empty")
  }

  test("modify") {
    (EPTraversal.list >>> PPrism.some).replace(true)(List(None, Some(2))) shouldEqual List(None, Some(true))
    (EPTraversal.list >>> PPrism.some).replace(true)(List(None, None)) shouldEqual List(None, None)
  }

}