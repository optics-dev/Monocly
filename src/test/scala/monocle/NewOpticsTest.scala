package monocle

import monocle.internal.NonEmptyList
import functions.Index
import Fixtures._


class NewOpticsTest extends munit.FunSuite {

  test("GetOne andThen GetOne") {
    val deskOptic: Optic[Get, Office, Desk] = 
      Optic.thatCan.get(_.desk)

    val pensOptic: Optic[Get, Desk, Int] = 
      Optic.thatCan.get(_.numPens)

    val composed: Optic[Get, Office, Int] = 
      deskOptic.andThen(pensOptic)

    val office = Office(Desk(5, None), Nil)
    assertEquals(composed.get(office), 5)
  }

  test("GetOption andThen GetOne") {
    val printerOptic: Optic[GetOption, Desk, Printer] = 
      Optic.thatCan.getOption(_.printer)

    val paperJamOptic: Optic[Get, Printer, Boolean] = 
      Optic.thatCan.get(_.paperJam)

    val composed: Optic[GetOption, Desk, Boolean] = 
      printerOptic.andThen(paperJamOptic)

    val desk = Desk(5, Some(Printer(true)))
    assertEquals(composed.getOption(desk), Some(true))
  }

  test("GetMany andThen GetOne") {
    val pensOptic: Optic[GetMany, Office, Pen] = 
      Optic.thatCan.getMany(_.pens)

    val colorOptic: Optic[Get, Pen, String] = 
      Optic.thatCan.get(_.color)

    val composed: Optic[GetMany, Office, String] = 
      pensOptic.andThen(colorOptic)

    val office = Office(Desk(5, None), List(Pen("red"), Pen("green"), Pen("blue")))
    assertEquals(composed.getAll(office), List("red", "green", "blue"))
  }

  test("Modify andThen Modify") {

    val modifyPrinter: Optic[Modify, Desk, Printer] = 
      Optic.thatCan.modify(f => desk => desk.copy(printer = desk.printer.map(f)))

    val modifyPaperJam: Optic[Modify, Printer, Boolean] = 
      Optic.thatCan.modify(f => printer => printer.copy(paperJam = f(printer.paperJam)))

    val composed: Optic[Modify, Desk, Boolean] = 
      modifyPrinter.andThen(modifyPaperJam)

    val desk = Desk(5, Some(Printer(true)))
    val desk2 = composed.modify(b => !b)(desk)

    assertEquals(desk2.printer.map(_.paperJam), Some(false))
  }

  test("ReverseGet andThen Modify") {

    val modifyPens: Optic[Modify, Office, Pen] = 
      Optic.thatCan.modify(f => office => office.copy(pens = office.pens.map(f)))

    val reverseGetColor: Optic[ReverseGet, Pen, String] = 
      Optic.thatCan.convertBetween[Pen, String](_.color)(Pen(_))

    val composed: Optic[Modify, Office, String] = 
      modifyPens.andThen(reverseGetColor)

    val office = Office(Desk(5, None), List(Pen("blue"), Pen("yellow")))
    val office2 = composed.replace("purple")(office)

    assertEquals(office2.pens.map(_.color), List("purple", "purple"))
  }

  test("Lens andThen GetOne") {
    val deskOptic: Optic[Get & Modify, Office, Desk] = 
      Optic.thatCan.edit[Office, Desk](_.desk)(d => _.copy(desk = d))

    val pensOptic: Optic[Get, Desk, Int] = 
      Optic.thatCan.get(_.numPens)

    val composed: Optic[Get, Office, Int] = 
      deskOptic.andThen(pensOptic)

    val office = Office(Desk(5, None), Nil)
    assertEquals(composed.get(office), 5)
  }

  test("Lens andThen Lens") {
    val deskOptic: Optic[Get & Modify, Office, Desk] = 
      Optic.thatCan.edit[Office, Desk](_.desk)(d => _.copy(desk = d))

    val pensOptic: Optic[Get & Modify, Desk, Int] = 
      Optic.thatCan.edit[Desk, Int](_.numPens)(n => _.copy(numPens = n))

    val composed: Optic[Get & Modify, Office, Int] = 
      deskOptic.andThen(pensOptic)

    val office = Office(Desk(55, None), Nil)
    val office2 = composed.modify(_ + 1)(office)

    assertEquals(composed.get(office), 55)
    assertEquals(office2.desk.numPens, 56)
  }

  test("GetOneOrMore andThen GetOne") {
    val pensOptic: Optic[GetOneOrMore, Office, Pen] = 
      Optic.thatCan.getOneOrMore(o => NonEmptyList(Pen("rainbow"), o.pens))

    val colorOptic: Optic[Get, Pen, String] = 
      Optic.thatCan.get(_.color)

    val composed: Optic[GetOneOrMore, Office, String] = 
      pensOptic.andThen(colorOptic)

    val office = Office(Desk(5, None), List(Pen("red"), Pen("green"), Pen("blue")))
    assertEquals(composed.getAll(office), List("rainbow", "red", "green", "blue"))
  }

  test("GetOneOrMore andThen GetOption") {
    val pensOptic: Optic[GetOneOrMore, Office, Pen] = 
      Optic.thatCan.getOneOrMore(o => NonEmptyList(Pen("rainbow", Some(Company("Hemingsworth"))), o.pens))

    val manufacturerOptic: Optic[GetOption, Pen, Company] = 
      Optic.thatCan.getOption(_.manufacturer)

    val composed: Optic[GetMany, Office, Company] = 
      pensOptic.andThen(manufacturerOptic)

    val office = Office(Desk(5, None), List(Pen("red", Some(Company("Pencorp"))), Pen("green"), Pen("blue")))
    assertEquals(composed.getAll(office), List(Company("Hemingsworth"), Company("Pencorp")))
  }
}