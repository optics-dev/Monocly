package optics.poly

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import scala.language.implicitConversions
import optics.poly.Optional

class ErrorCompositionTest extends AnyFunSuite with Matchers {

  test("compose with different errors") {
    val some3 = EPPrism.some("err") >>> EPPrism.some(2) >>> EPPrism.some[Boolean, Int, String](true)

    some3.getOrError(Some(Some(Some(1)))) shouldEqual Right(1)
    some3.getOrError(Some(Some(None))) shouldEqual Left(true)
    some3.getOrError(Some(None)) shouldEqual Left(2)
    some3.getOrError(None) shouldEqual Left("err")
  }

}
