package optics.poly

import scala.language.implicitConversions

class ErrorCompositionTest extends munit.FunSuite {

  test("compose with different errors") {
    val some3 = EPPrism.some("err")
      .andThen(EPPrism.some(2))
      .andThen(EPPrism.some[Boolean, Int, String](true))

    assertEquals(some3.getOrError(Some(Some(Some(1)))), Right(1))
    assertEquals(some3.getOrError(Some(Some(None))), Left(true))
    assertEquals(some3.getOrError(Some(None)), Left(2))
    assertEquals(some3.getOrError(None), Left("err"))
  }

  test("traversal errors level 1") {
    assertEquals(EPTraversal.list.toListOrError(List(1, 2)), Right(List(1, 2)))
    assertEquals(EPTraversal.list.toListOrError(Nil), Left("List is empty"))
  }

  test("traversal errors level 2") {
    assertEquals(EPTraversal.list.andThen(PPrism.some).toListOrError(List(Some(1), Some(2))), Right(List(1, 2)))
    assertEquals(EPTraversal.list.andThen(PPrism.some).toListOrError(List(None, Some(2))), Right(List(2)))
    assertEquals(EPTraversal.list.andThen(PPrism.some).toListOrError(List(None, None)), Left("List is empty"))
  }

}
