package optics.poly

import scala.language.implicitConversions
import Optional.indexMap

class OptionalTest extends munit.FunSuite {

  case class Foo[A](i: Int, opt: Option[A])

  def opt[A,  B] = EPOptional[String, Foo[A], Foo[B], A, B](
    from => from.opt.toRight(("Missing opt", from.copy(opt = None))),
    newValue => from => from.copy(opt = from.opt.map(_ => newValue))
  )

  test("opt") {
    val pos = Foo(4, Some(true))
    val neg = Foo(4, None)

    assert(opt.getOrError(pos) == Right(true))
    assert(opt.getOrError(neg) == Left("Missing opt"))

    assert(opt.replace(List(1,2))(pos) == Foo(4, Some(List(1,2))))
    assert(opt.replace(List(1,2))(neg) == neg)
  }

  test("indexMap") {
    val map = Map("foo" -> Map(1 -> true, 2 -> false), "bar" -> Map(0 -> true))

    assert((indexMap("foo") >>> indexMap(1)).getOrError(map) == Right(true))
    assert((indexMap("foo") >>> indexMap(0)).getOrError(map) == Left("Key 0 is missing"))
    assert((indexMap("zzz") >>> indexMap(1)).getOrError(map) == Left("Key zzz is missing"))

    assert((indexMap("foo") >>> indexMap(1)).replace(false)(map) ==  Map("foo" -> Map(1 -> false, 2 -> false), "bar" -> Map(0 -> true)))
  }

}
