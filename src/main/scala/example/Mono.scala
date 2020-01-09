package example

import optics._

case class Mono(i: Int, foo: Foo, optI: Option[Int])

object Mono {
  val i: Lens[Mono, Int] = Lens[Mono, Int](_.i)((newI, bar) => bar.copy(i = newI))
  val foo: Lens[Mono, Foo] = Lens[Mono, Foo](_.foo)((newFoo, bar) => bar.copy(foo = newFoo))
  val optI: Lens[Mono, Option[Int]] = Lens[Mono, Option[Int]](_.optI)((newOptI, bar) => bar.copy(optI = newOptI))
}

case class Poly[A](i: Int, foo: Foo, optI: Option[Int], param: A)

object Poly {
  def i[A]: Lens[Poly[A], Int] = Lens[Poly[A], Int](_.i)((newVal, bar) => bar.copy(i = newVal))
  def foo[A]: Lens[Poly[A], Foo] = Lens[Poly[A], Foo](_.foo)((newVal, bar) => bar.copy(foo = newVal))
  def optI[A]: Lens[Poly[A], Option[Int]] = Lens[Poly[A], Option[Int]](_.optI)((newVal, bar) => bar.copy(optI = newVal))
  def param[A, B]: PLens[Poly[A], Poly[B], A, B] = PLens[Poly[A], Poly[B], A, B](_.param)((newVal, bar) => bar.copy(param = newVal))
}

sealed trait Foo {
  def asFooI: Option[Foo.FooI] = this match {
    case x: Foo.FooI => Some(x)
    case _ => None
  }

  def asFooIS: Option[Foo.FooIS] = this match {
    case x: Foo.FooIS => Some(x)
    case _ => None
  }
}

object Foo {
  case class FooI(i: Int) extends Foo
  case class FooS(s: String) extends Foo
  case class FooIS(i: Int, s: String) extends Foo
  case object FooU extends Foo

  val fooI: Prism[Foo, FooI] = Prism.partial[Foo, FooI]{ case x: FooI => x }(identity)
  val fooS: Prism[Foo, FooS] = Prism.partial[Foo, FooS]{ case x: FooS => x }(identity)
  val fooIS: Prism[Foo, FooIS] = Prism.partial[Foo, FooIS]{ case x: FooIS => x }(identity)
  val fooU: Prism[Foo, FooU.type] = Prism.partial[Foo, FooU.type]{ case FooU => FooU }(identity)

  object FooIS {
    val i: Lens[FooIS, Int] = Lens[FooIS, Int](_.i)((newI, foo) => foo.copy(i = newI))
  }
}
