package optics.poly

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.language.implicitConversions
import optics.poly.Optional

class OptionalTest extends AnyFunSuite with Matchers {

  case class Foo[A](i: Int, opt: Option[A])

  def opt[A,  B] = EPOptional[String, Foo[A], Foo[B], A, B](
    from => from.opt.toRight(("Missing opt", from.copy(opt = None))),
    newValue => from => from.copy(opt = from.opt.map(_ => newValue))
  )

  test("opt") {
    val pos = Foo(4, Some(true))
    val neg = Foo(4, None)

    opt.getOrError(pos) shouldEqual Right(true)
    opt.getOrError(neg) shouldEqual Left("Missing opt")

    opt.replace(List(1,2))(pos) shouldEqual Foo(4, Some(List(1,2)))
    opt.replace(List(1,2))(neg) shouldEqual neg
  }

}
