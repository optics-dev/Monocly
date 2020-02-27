package optics.mono

import optics.mono.tc._

import scala.language.implicitConversions

class IndexTest extends munit.FunSuite {

  case class Foo(list: List[String])

  val list: Lens[Foo, List[String]] = Lens(_.list, xs => _.copy(list = xs))
  val foo = Foo(List("a", "b", "c"))

  test("index1") {
    assert((list >>> Index1.index(1)).getOrError(foo) == Right(1))
  }

  test("index2") {
    assert((list >>> Index2.index(1)).getOrError(foo) == Right(1))
  }

}
