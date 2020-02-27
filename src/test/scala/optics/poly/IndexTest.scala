package optics.poly

import scala.language.implicitConversions
import optics.poly.tc.Index1.index

class IndexTest extends munit.FunSuite {

  case class Foo(list: List[String])

  val list: Lens[Foo, List[String]] = Lens(_.list, xs => _.copy(list = xs))

  test("indexMap") {
    val foo = Foo(List("a", "b", "c"))

//    assert((list >>> index(1)).getOrError(foo) == Right(1))
  }

}
