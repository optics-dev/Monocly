package optics.poly

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.language.implicitConversions
import optics.poly.Optional

class TypeInferenceTest extends AnyFunSuite with Matchers {

  test("some polymorphic update level 1") {
    PPrism.some.replace(2)(Some(true)) shouldEqual Some(2)
    PPrism.some.replace(2)(Option(true)) shouldEqual Some(2)
  }

  test("some polymorphic update level 2") {
    (PPrism.some >>> PPrism.some).replace(2)(Some(Some(true))) shouldEqual Some(Some(2))
    (PPrism.some >>> PPrism.some).replace(2)(Option(Option(true))) shouldEqual Some(Some(2))
  }

  test("_1 polymorphic update level 1") {
    PLens._1.replace(0)((false, "foo")) shouldEqual (0, "foo")
  }

  test("_1 polymorphic update level 2") {
    (PLens._1 >>> PLens._1).replace(0)(((false, 7), "foo")) shouldEqual ((0, 7), "foo")
  }

  test("compose Lens and Prism") {
    (PLens._1 >>> PPrism.some >>> PLens._2).replace(0)((Some((true, "foo")), 10L)) shouldEqual (Some((true, 0)), 10L)
  }

}
