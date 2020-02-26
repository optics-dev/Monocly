package optics.poly

import scala.language.implicitConversions

class TypeInferenceTest extends munit.FunSuite {

  test("some polymorphic update level 1") {
    assert(PPrism.some.replace(2)(Some(true)) == Some(2))
    assert(PPrism.some.replace(2)(Option(true)) == Some(2))
  }

  test("some polymorphic update level 2") {
    assert((PPrism.some >>> PPrism.some).replace(2)(Some(Some(true))) == Some(Some(2)))
    assert((PPrism.some >>> PPrism.some).replace(2)(Option(Option(true))) == Some(Some(2)))
  }

  test("_1 polymorphic update level 1") {
    assertEquals(PLens._1.replace(0)((false, "foo")), (0, "foo"))
  }

  test("_1 polymorphic update level 2") {
    assertEquals((PLens._1 >>> PLens._1).replace(0)(((false, 7), "foo")), ((0, 7), "foo"))
  }

  test("compose Lens and Prism") {
    assert((PLens._1 >>> PPrism.some >>> PLens._2).replace(0)((Some((true, "foo")), 10L)) == (Some((true, 0)), 10L))
  }

}
