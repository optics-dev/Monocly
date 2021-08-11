package optics.poly

import functions.Index
/*
class IndexTest extends munit.FunSuite {

  case class Foo(i: Int)
  case class Bar(l: List[Foo], m: Map[String, Foo])

  val lLens = Lens[Bar, List[Foo]](_.l, ll => _.copy(l = ll))
  val mLens = Lens[Bar, Map[String, Foo]](_.m, mm => _.copy(m = mm))
  val iLens = Lens[Foo, Int](_.i, ii => _.copy(i = ii))


  test("inference") {
    assertEquals(
      lLens.index(0).andThen(iLens).getOption(Bar(List(Foo(1)), Map())),
      Some(1)
    )
    assertEquals(
      mLens.index("moo").andThen(iLens).getOption(Bar(List(), Map("moo" -> Foo(1)))),
      Some(1)
    )
  }

}
*/