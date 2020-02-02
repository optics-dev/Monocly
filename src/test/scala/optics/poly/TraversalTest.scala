package optics.poly


import scala.language.implicitConversions

class TraversalTest extends munit.FunSuite {

  test("toListOrError") {
    assert(NonEmptyPTraversal.pair.toListOrError((5, 6)) == Right(List(5, 6)))

    assert(EPTraversal.list.toListOrError(List(1,2,3)) == Right(List(1,2,3)))
    assert(EPTraversal.list.toListOrError(Nil) == Left("List is empty"))
  }

  test("modify") {
    assertEquals((EPTraversal.list >>> PPrism.some).replace(true)(List(None, Some(2))), List(None, Some(true)))
    assert((EPTraversal.list >>> PPrism.some).replace(true)(List(None, None)) == List(None, None))
  }

}