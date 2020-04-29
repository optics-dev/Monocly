package optics.poly

import scala.language.implicitConversions
import ops._
import functions.Index

class OptionalTest extends munit.FunSuite {

  case class Foo[A](i: Int, opt: Option[A])

  def optLens[A, B]: PLens[Foo[A], Foo[B], Option[A], Option[B]] =
    PLens(_.opt, optB => _.copy(opt = optB))

  def opt[A,  B] = EPOptional[String, Foo[A], Foo[B], A, B](
    from => from.opt.toRight(("Missing opt", from.copy(opt = None))),
    newValue => from => Right(from.copy(opt = from.opt.map(_ => newValue)))
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

  case class Bar(listI: List[Int])

  val listI: Lens[Bar, List[Int]] = ???

  test("map") {
    val map = Map("foo" -> Map(1 -> true, 2 -> false), "bar" -> Map(0 -> true))

    (listI >>> Index(1)).replace(3)

    assertEquals((Index.map("foo") >>> Index.map(1)).getOrError(map), Right(true))
    // assertEquals((Index.map("foo") >>> indexMap(0)).getOrError(map), Left("Key 0 is missing"))
    // assertEquals((Index.map("zzz") >>> indexMap(1)).getOrError(map), Left("Key zzz is missing"))
    assertEquals((Index.map("foo") >>> Index.map(1)).replace(false)(map),  Map("foo" -> Map(1 -> false, 2 -> false), "bar" -> Map(0 -> true)))
  }

}
