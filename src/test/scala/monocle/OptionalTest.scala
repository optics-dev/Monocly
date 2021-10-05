package monocle

import scala.language.implicitConversions
import functions.Index
import monocle.classic.*

class OptionalTest extends munit.FunSuite {

  case class Foo[A](i: Int, opt: Option[A])

  def optLens[A, B]: PLens[Foo[A], Foo[B], Option[A], Option[B]] =
    PLens(_.opt, optB => _.copy(opt = optB))

  def opt[A,  B] = POptional[Foo[A], Foo[B], A, B](
    from => from.opt.toRight(from.copy(opt = None)),
    newValue => from => from.copy(opt = from.opt.map(_ => newValue))
  )

  test("opt") {
    val pos = Foo(4, Some(true))
    val neg = Foo(4, None)

    assertEquals(opt.getOption(pos), Some(true))
    assertEquals(opt.getOption(neg), None)

    assertEquals(opt.replace(List(1,2))(pos), Foo(4, Some(List(1,2))))
    assertEquals[Any, Any](opt.replace(List(1,2))(neg), neg)
  }

  test("some") {
    val pos = Foo(4, Some(true))
    val neg = Foo(4, None)

    assertEquals(optLens.some.getOption(pos), Some(true))
    assertEquals(optLens.some.getOption(neg), None)
  }

  test("map") {
    val map = Map("foo" -> Map(1 -> true, 2 -> false), "bar" -> Map(0 -> true))

    assertEquals(Index.map("foo").andThen(Index.map(1)).getOption(map), Some(true))
    assertEquals(Index.map("foo").andThen(Index.map(0)).getOption(map), None)
    assertEquals(Index.map("zzz").andThen(Index.map(1)).getOption(map), None)
    assertEquals(Index.map("foo").andThen(Index.map(1)).replace(false)(map),  Map("foo" -> Map(1 -> false, 2 -> false), "bar" -> Map(0 -> true)))
  }

}
