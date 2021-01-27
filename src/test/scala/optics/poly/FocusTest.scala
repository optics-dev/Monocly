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
  case class Box[A](a: A) 
  case class MultiBox[A,B](a: A, b: B)
  case class HigherBox[F[_], A](fa: F[A])
  case class RefinedBox[A](a: A) {type Banana}
  case class UnionBox[A,B](aOrB: A | B)
  case class ConstraintBox[A <: AnyVal](a: A)
  case class Varargs[A](a: A*)

  test("Single field access") {
    assertEquals(
      Focus[Animal](_.name).get(Animal("Bob")),
      "Bob"
    )
  }

  test("Nested field access") {
    assertEquals(
      Focus[Shop](_.owner.pet.name).get(Shop(Owner(Animal("Fred")))),
      "Fred"
    )
  }

  test("Type parameter field access") {
    assertEquals(
      Focus[Box[String]](_.a).get(Box("Hello")),
      "Hello"
    )
  }

  test("Type parameter set field") {
    assertEquals(
      Focus[Box[Int]](_.a).replace(111)(Box(222)),
      Box(111)
    )
  }

  test("Nested type parameter set field") {
    assertEquals(
      Focus[Box[Box[String]]](_.a.a).replace("hello")(Box(Box("ok"))),
      Box(Box("hello"))
    )
  }

  test("Multiple type parameters get field") {
    assertEquals(
      Focus[MultiBox[Int, Boolean]](_.b).get(MultiBox(222, true)),
      true
    )
  }

  test("Multiple type parameters set field") {
    assertEquals(
      Focus[MultiBox[String, Int]](_.a).replace("abc")(MultiBox("whatevs",222)),
      MultiBox("abc", 222)
    )
  }

  /*
  // We can't support this yet, because we don't know how to replace the type variables inside the `fa` value.
  test("Higher kinded type parameter get field") {
    assertEquals(
      Focus[HigherBox[Option, Int]](_.fa).get(HigherBox(Some(23))),
      Some(23)
    )
  }*/

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
