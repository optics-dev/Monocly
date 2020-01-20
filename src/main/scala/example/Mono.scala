package example

import optics._
import optics.poly.PLens

case class Mono(i: Int, foo: Foo, optI: Option[Int])

object Mono {
  val i: Lens[Mono, Int] = Lens[Mono, Int](_.i, newVal => _.copy(i = newVal))
  val foo: Lens[Mono, Foo] = Lens[Mono, Foo](_.foo, newVal => _.copy(foo = newVal))
  val optI: Lens[Mono, Option[Int]] = Lens[Mono, Option[Int]](_.optI, newVal => _.copy(optI = newVal))
}

case class Poly[A](i: Int, foo: Foo, optI: Option[Int], param: A)

object Poly {
  def i[A]: Lens[Poly[A], Int] = Lens[Poly[A], Int](_.i, newVal => _.copy(i = newVal))
  def foo[A]: Lens[Poly[A], Foo] = Lens[Poly[A], Foo](_.foo, newVal => _.copy(foo = newVal))
  def optI[A]: Lens[Poly[A], Option[Int]] = Lens[Poly[A], Option[Int]](_.optI, newVal => _.copy(optI = newVal))
  def param[A, B]: PLens[Poly[A], Poly[B], A, B] = PLens[Poly[A], Poly[B], A, B](_.param)(newVal => _.copy(param = newVal))
}

sealed trait Foo

object Foo {
  case class FooI(i: Int) extends Foo
  case class FooS(s: String) extends Foo
  case class FooIS(i: Int, s: String) extends Foo
  case class FooList(l: List[Int]) extends Foo
  case object FooU extends Foo

  def mkError[A](expected: String): A => String = a => s"Expected $expected but got $a"

  val fooI: EPrism[String, Foo, FooI] = EPrism.partial[String, Foo, FooI]{ case x: FooI => x }(mkError("FooI"))(identity)
  val fooS: EPrism[String, Foo, FooS] = EPrism.partial[String, Foo, FooS]{ case x: FooS => x }(mkError("FooS"))(identity)
  val fooIS: EPrism[String, Foo, FooIS] = EPrism.partial[String, Foo, FooIS]{ case x: FooIS => x }(mkError("FooIS"))(identity)
  val fooU: EPrism[String, Foo, FooU.type] = EPrism.partial[String, Foo, FooU.type]{ case FooU => FooU }(mkError("FooU"))(identity)

  val fooL: EPrism[String, Foo, List[Int]] = EPrism.partial[String, Foo, List[Int]]{ case FooList(x) => x }(mkError("FooList"))(FooList(_))

  object FooIS {
    val i: Lens[FooIS, Int] = Lens[FooIS, Int](_.i, newVal => _.copy(i = newVal))
  }
}
