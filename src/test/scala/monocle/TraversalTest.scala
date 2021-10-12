package monocle

import scala.language.implicitConversions
import monocle.classic.*

class TraversalTest extends munit.FunSuite {

//  test("toListOrError") {
//    assertEquals(Traversal.pair.toListOrError((5, 6)), Right(List(5, 6)))

//    assertEquals(EPTraversal.list.toListOrError(List(1,2,3)), Right(List(1,2,3)))
//    assertEquals(EPTraversal.list.toListOrError(Nil), Left("List is empty"))
//  }

  test("modify") {
    assertEquals(PTraversal.list.andThen(PPrism.some).replace(true)(List(None, Some(2))), List(None, Some(true)))
    assertEquals(PTraversal.list.andThen(PPrism.some).replace(true)(List(None, None)), List(None, None))
  }

}
