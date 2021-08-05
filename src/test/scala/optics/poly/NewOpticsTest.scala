package optics.poly

import functions.Index

class NewOpticsTest extends munit.FunSuite {

  case class Pen(color: String)
  case class Office(desk: Desk, pens: List[Pen])
  case class Printer(pcLoadLetter: Boolean)
  case class Desk(numPens: Int, printer: Option[Printer])


  test("GetOne andThen GetOne") {
    val deskOptic: Optic[GetOne, Office, Desk] = 
      Optic.withGetOne(_.desk)

    val pensOptic: Optic[GetOne, Desk, Int] = 
      Optic.withGetOne(_.numPens)

    val composed: Optic[GetOne, Office, Int] = 
      deskOptic.andThen(pensOptic)

    val office = Office(Desk(5, None), Nil)
    assertEquals(composed.get(office), 5)
  }

  test("GetOption andThen GetOne") {
    val printerOptic: Optic[GetOption, Desk, Printer] = 
      Optic.withGetOption(_.printer)

    val pcLoadLetterOptic: Optic[GetOne, Printer, Boolean] = 
      Optic.withGetOne(_.pcLoadLetter)

    val composed: Optic[GetOption, Desk, Boolean] = 
      printerOptic.andThen(pcLoadLetterOptic)

    val desk = Desk(5, Some(Printer(true)))
    assertEquals(composed.getOption(desk), Some(true))
  }

  test("GetMany andThen GetOne") {
    val pensOptic: Optic[GetMany, Office, Pen] = 
      Optic.withGetMany(_.pens)

    val colorOptic: Optic[GetOne, Pen, String] = 
      Optic.withGetOne(_.color)

    val composed: Optic[GetMany, Office, String] = 
      pensOptic.andThen(colorOptic)

    val office = Office(Desk(5, None), List(Pen("red"), Pen("green"), Pen("blue")))
    assertEquals(composed.getAll(office), List("red", "green", "blue"))
  }

}
