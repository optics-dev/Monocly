package example

import optics._

case class Bar(i: Int, id: Id, foo: Foo, optI: Option[Int])

object Bar {
  val i: Lens[Bar, Int] = Lens[Bar, Int](_.i)((bar, newI) => bar.copy(i = newI))
  val id: Lens[Bar, Id] = Lens[Bar, Id](_.id)((bar, newId) => bar.copy(id = newId))
  val foo: Lens[Bar, Foo] = Lens[Bar, Foo](_.foo)((bar, newFoo) => bar.copy(foo = newFoo))
  val optI: Lens[Bar, Option[Int]] = Lens[Bar, Option[Int]](_.optI)((bar, newOptI) => bar.copy(optI = newOptI))
}

case class Id(x: String)

object Id {
  val x: Iso[Id, String] = Iso[Id, String](_.x)(Id(_))
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
    val i: Lens[FooIS, Int] = Lens[FooIS, Int](_.i)((foo, newI) => foo.copy(i = newI))
  }
}
