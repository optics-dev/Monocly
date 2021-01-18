package optics.poly


import scala.language.implicitConversions

class TraversalTest extends munit.FunSuite {

  test("toListOrError") {
    assertEquals(NonEmptyPTraversal.pair.toListOrError((5, 6)), Right(List(5, 6)))

    assertEquals(EPTraversal.list.toListOrError(List(1,2,3)), Right(List(1,2,3)))
    assertEquals(EPTraversal.list.toListOrError(Nil), Left("List is empty"))
  }

  test("modify") {
    assertEquals(EPTraversal.list.andThen(PPrism.some).replace(true)(List(None, Some(2))), List(None, Some(true)))
    assertEquals(EPTraversal.list.andThen(PPrism.some).replace(true)(List(None, None)), List(None, None))
  }

}