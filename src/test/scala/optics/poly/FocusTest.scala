package optics.poly

import dsl._

final class FocusTest extends munit.FunSuite {

  case class Fub(bab: Int)
  case class Bar(fub: Fub)
  case class Foo(bar: Option[Bar])
  case class Qux(foo: Either[String, Foo], moo: Map[Int, Fub])

  case class Animal(name: String)
  case class Owner(pet: Animal)
  case class Shop(owner: Owner)

  val fooLens: EOptional[NoSuchElementException, Shop, String] =
    Focus[Shop](_.owner.pet.name)

  test("Single field access") {
    assertEquals(
      Focus[Animal](_.name).getOrError(Animal("Bob")),
      Right("Bob")
    )
  }

  test("Nested field access") {
    assertEquals(
      Focus[Shop](_.owner.pet.name).getOrError(Shop(Owner(Animal("Fred")))),
      Right("Fred")
    )
  }

  /*
  val fooLens: EOptional[NoSuchElementException, Foo, Int] =
    Focus[Foo](_.bar.?.fub.bab)

  test("compose focus with `?`") {
    assertEquals(
      fooLens.getOrError(Foo(Some(Bar(Fub(1))))),
      Right(1)
    )
  }

  val quxLens: EOptional[NoSuchElementException | String, Qux, Int] =
    Focus[Qux](_.foo.?.bar.?.fub.bab)

  test("compose focus with nested `?`") {
    assertEquals(
      quxLens.getOrError(Qux(Right(Foo(Some(Bar(Fub(1))))), Map())),
      Right(1)
    )
  }


  test("compose focus with index") {
    val lens: EOptional[NoSuchElementException, Qux, Int] = Focus[Qux](_.moo.idx(32).bab)
    assertEquals(
      lens.getOrError(Qux(Left("moo"), Map(32 -> Fub(32)))),
      Right(32)
    )
  }

  test("mixture of operators") {
    val lens = Focus[Map[String, Qux]](_.idx("moo").foo.?.bar.?.fub)
    assertEquals(
      lens.getOrError(Map("moo" -> Qux(Right(Foo(Some(Bar(Fub(21))))), Map()))),
      Right(Fub(21))
    )
  }*/

}
