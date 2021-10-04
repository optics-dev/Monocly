package example

import monocly._
import monocly.classic.*

case class Mono(i: Int, foo: Foo, optI: Option[Int], listI: List[Int], mapI: Map[Int, Int])

object Mono {
  val i: Lens[Mono, Int] = Lens[Mono, Int](_.i, newVal => _.copy(i = newVal))
  val foo: Lens[Mono, Foo] = Lens[Mono, Foo](_.foo, newVal => _.copy(foo = newVal))
  val optI: Lens[Mono, Option[Int]] = Lens[Mono, Option[Int]](_.optI, newVal => _.copy(optI = newVal))
  val listI: Lens[Mono, List[Int]] = Lens[Mono, List[Int]](_.listI, newVal => _.copy(listI = newVal))
  val mapI: Lens[Mono, Map[Int, Int]] = Lens[Mono, Map[Int, Int]](_.mapI, newVal => _.copy(mapI = newVal))
}

case class Poly[A](i: Int, foo: Foo, optI: Option[Int], param: A)

object Poly {
  def i[A]: Lens[Poly[A], Int] = Lens[Poly[A], Int](_.i, newVal => _.copy(i = newVal))
  def foo[A]: Lens[Poly[A], Foo] = Lens[Poly[A], Foo](_.foo, newVal => _.copy(foo = newVal))
  def optI[A]: Lens[Poly[A], Option[Int]] = Lens[Poly[A], Option[Int]](_.optI, newVal => _.copy(optI = newVal))
  def param[A]: Lens[Poly[A], A] = Lens[Poly[A], A](_.param, newVal => _.copy(param = newVal))
  def pparam[A, B]: PLens[Poly[A], Poly[B], A, B] = PLens[Poly[A], Poly[B], A, B](_.param, newVal => _.copy(param = newVal))
}

sealed trait Foo

object Foo {
  case class FooI(i: Int) extends Foo
  case class FooS(s: String) extends Foo
  case class FooIS(i: Int, s: String) extends Foo
  case class FooList(l: List[Int]) extends Foo
  case object FooU extends Foo

  def mkError[A](expected: String): A => String = a => s"Expected $expected but got $a"

  val fooI: Prism[Foo, FooI] = Prism.partial[Foo, FooI]{ case x: FooI => x }(identity)
  val fooS: Prism[Foo, FooS] = Prism.partial[Foo, FooS]{ case x: FooS => x }(identity)
  val fooIS: Prism[Foo, FooIS] = Prism.partial[Foo, FooIS]{ case x: FooIS => x }(identity)
  val fooU: Prism[Foo, FooU.type] = Prism.partial[Foo, FooU.type]{ case FooU => FooU }(identity)

  val fooL: Prism[Foo, List[Int]] = Prism.partial[Foo, List[Int]]{ case FooList(x) => x }(FooList(_))

  object FooIS {
    val i: Lens[FooIS, Int] = Lens[FooIS, Int](_.i, newVal => _.copy(i = newVal))
  }
}
