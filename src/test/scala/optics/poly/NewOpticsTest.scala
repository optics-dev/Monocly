package monocly

import monocly.internal.NonEmptyList
import functions.Index

class NewOpticsTest extends munit.FunSuite {

  case class Company(name: String)
  case class Pen(color: String, manufacturer: Option[Company] = None)
  case class Office(desk: Desk, pens: List[Pen])
  case class Printer(pcLoadLetter: Boolean)
  case class Desk(numPens: Int, printer: Option[Printer])


  test("GetOne andThen GetOne") {
    val deskOptic: Optic[GetOne, Office, Desk] = 
      OpticsBuilder.withGetOne(_.desk)

    val pensOptic: Optic[GetOne, Desk, Int] = 
      OpticsBuilder.withGetOne(_.numPens)

    val composed: Optic[GetOne, Office, Int] = 
      deskOptic.andThen(pensOptic)

    val office = Office(Desk(5, None), Nil)
    assertEquals(composed.get(office), 5)
  }

  test("GetOption andThen GetOne") {
    val printerOptic: Optic[GetOption, Desk, Printer] = 
      OpticsBuilder.withGetOption(_.printer)

    val pcLoadLetterOptic: Optic[GetOne, Printer, Boolean] = 
      OpticsBuilder.withGetOne(_.pcLoadLetter)

    val composed: Optic[GetOption, Desk, Boolean] = 
      printerOptic.andThen(pcLoadLetterOptic)

    val desk = Desk(5, Some(Printer(true)))
    assertEquals(composed.getOption(desk), Some(true))
  }

  test("GetMany andThen GetOne") {
    val pensOptic: Optic[GetMany, Office, Pen] = 
      OpticsBuilder.withGetMany(_.pens)

    val colorOptic: Optic[GetOne, Pen, String] = 
      OpticsBuilder.withGetOne(_.color)

    val composed: Optic[GetMany, Office, String] = 
      pensOptic.andThen(colorOptic)

    val office = Office(Desk(5, None), List(Pen("red"), Pen("green"), Pen("blue")))
    assertEquals(composed.getAll(office), List("red", "green", "blue"))
  }

  test("Modify andThen Modify") {

    val modifyPrinter: Optic[Modify, Desk, Printer] = 
      OpticsBuilder.withModify(f => desk => desk.copy(printer = desk.printer.map(f)))
    val modifyPcLoadLetter: Optic[Modify, Printer, Boolean] = 
      OpticsBuilder.withModify(f => printer => printer.copy(pcLoadLetter = f(printer.pcLoadLetter)))

    val composed: Optic[Modify, Desk, Boolean] = 
      modifyPrinter.andThen(modifyPcLoadLetter)

    val desk = Desk(5, Some(Printer(true)))
    val desk2 = composed.modify(b => !b)(desk)

    assertEquals(desk2.printer.map(_.pcLoadLetter), Some(false))
  }

  test("ReverseGet andThen Modify") {

    val modifyPens: Optic[Modify, Office, Pen] = 
      OpticsBuilder.withModify(f => office => office.copy(pens = office.pens.map(f)))

    val reverseGetColor: Optic[ReverseGet, Pen, String] = 
      OpticsBuilder.withReverseGet[Pen, Pen, String, String](f => pen => pen.copy(color = f(pen.color)), Pen(_))

    val composed: Optic[Modify, Office, String] = 
      modifyPens.andThen(reverseGetColor)

    val office = Office(Desk(5, None), List(Pen("blue"), Pen("yellow")))
    val office2 = composed.replace("purple")(office)

    assertEquals(office2.pens.map(_.color), List("purple", "purple"))
  }

  test("Lens andThen GetOne") {
    val deskOptic: Optic[GetOne & Modify, Office, Desk] = 
      OpticsBuilder.withLens[Office, Desk](_.desk)(d => _.copy(desk = d))

    val pensOptic: Optic[GetOne, Desk, Int] = 
      OpticsBuilder.withGetOne(_.numPens)

    val composed: Optic[GetOne, Office, Int] = 
      deskOptic.andThen(pensOptic)

    val office = Office(Desk(5, None), Nil)
    assertEquals(composed.get(office), 5)
  }

  test("Lens andThen Lens") {
    val deskOptic: Optic[GetOne & Modify, Office, Desk] = 
      OpticsBuilder.withLens[Office, Desk](_.desk)(d => _.copy(desk = d))

    val pensOptic: Optic[GetOne & Modify, Desk, Int] = 
      OpticsBuilder.withLens[Desk, Int](_.numPens)(n => _.copy(numPens = n))

    val composed: Optic[GetOne & Modify, Office, Int] = 
      deskOptic.andThen(pensOptic)

    val office = Office(Desk(55, None), Nil)
    val office2 = composed.modify(_ + 1)(office)

    assertEquals(composed.get(office), 55)
    assertEquals(office2.desk.numPens, 56)
  }

  test("GetOneOrMore andThen GetOne") {
    val pensOptic: Optic[GetOneOrMore, Office, Pen] = 
      OpticsBuilder.withGetOneOrMore(o => NonEmptyList(Pen("rainbow"), o.pens))

    val colorOptic: Optic[GetOne, Pen, String] = 
      OpticsBuilder.withGetOne(_.color)

    val composed: Optic[GetOneOrMore, Office, String] = 
      pensOptic.andThen(colorOptic)

    val office = Office(Desk(5, None), List(Pen("red"), Pen("green"), Pen("blue")))
    assertEquals(composed.getAll(office), List("rainbow", "red", "green", "blue"))
  }

  test("GetOneOrMore andThen GetOption") {
    val pensOptic: Optic[GetOneOrMore, Office, Pen] = 
      OpticsBuilder.withGetOneOrMore(o => NonEmptyList(Pen("rainbow", Some(Company("Hemingsworth"))), o.pens))

    val manufacturerOptic: Optic[GetOption, Pen, Company] = 
      OpticsBuilder.withGetOption(_.manufacturer)

    val composed: Optic[GetMany, Office, Company] = 
      pensOptic.andThen(manufacturerOptic)

    val office = Office(Desk(5, None), List(Pen("red", Some(Company("Pencorp"))), Pen("green"), Pen("blue")))
    assertEquals(composed.getAll(office), List(Company("Hemingsworth"), Company("Pencorp")))
  }
}