package optics.poly

import scala.language.implicitConversions

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

}
