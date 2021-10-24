package monocle

import monocle.internal.{NonEmptyList, Id}
import functions.Index
import Fixtures._

class ComposeModify extends munit.FunSuite {

  test("Partially applied modify result in composable functions") {
    val composed: Desk => Desk = Fixtures.deskNumPens.modify(_ + 1) compose Fixtures.deskPrinter.replace(Printer(false))
    val result = composed(Fixtures.office.desk)

    assertEquals(result, Desk(5, Some(Printer(false))))
  }

  test("Partially applied modifyA result in composable functions") {
    case class Foo(ints: List[Int], strings: List[String])
    
    val ints: Optic[GetMany & Modify, Foo, Int] = 
      Optic.thatCan.editMany[Foo, Int, List](_.ints)(list => _.copy(ints = list))

    val strings = 
      Optic.thatCan.editMany[Foo, String, List](_.strings)(list => _.copy(strings = list))

    val composed: Foo => Foo = ints.modifyA[Id](_ + 1) compose strings.modifyA[Id](_.toUpperCase)
    val result = composed(Foo(List(1,2,3), List("a", "b", "c")))

    assertEquals(result, Foo(List(2,3,4), List("A", "B", "C")))
  }
}
