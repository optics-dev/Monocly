package optics.poly

import scala.language.implicitConversions
import EOptional.indexMap

class OptionalTest extends munit.FunSuite {

  case class Foo[A](i: Int, opt: Option[A])

  def optLens[A, B]: PLens[Foo[A], Foo[B], Option[A], Option[B]] =
    PLens(_.opt, optB => _.copy(opt = optB))

  def opt[A,  B] = EPOptional[String, Foo[A], Foo[B], A, B](
    from => from.opt.toRight(("Missing opt", from.copy(opt = None))),
    newValue => from => from.copy(opt = from.opt.map(_ => newValue))
  )

  test("opt") {
    val pos = Foo(4, Some(true))
    val neg = Foo(4, None)

    assertEquals(opt.getOrError(pos), Right(true))
    assertEquals(opt.getOrError(neg), Left("Missing opt"))

    assertEquals(opt.replace(List(1,2))(pos), Foo(4, Some(List(1,2))))
    assertEquals[Any, Any](opt.replace(List(1,2))(neg), neg)
  }

  test("some") {
    val pos = Foo(4, Some(true))
    val neg = Foo(4, None)

    assertEquals(optLens.some.getOrError(pos), Right(true))
    assertEquals(optLens.some.getOrError(neg), Left("None is not a Some"))
  }

  test("indexMap") {
    val map = Map("foo" -> Map(1 -> true, 2 -> false), "bar" -> Map(0 -> true))

    assertEquals((indexMap("foo") >>> indexMap(1)).getOrError(map), Right(true))
    assertEquals((indexMap("foo") >>> indexMap(0)).getOrError(map), Left("Key 0 is missing"))
    assertEquals((indexMap("zzz") >>> indexMap(1)).getOrError(map), Left("Key zzz is missing"))

    assertEquals((indexMap("foo") >>> indexMap(1)).replace(false)(map),  Map("foo" -> Map(1 -> false, 2 -> false), "bar" -> Map(0 -> true)))
  }

}
