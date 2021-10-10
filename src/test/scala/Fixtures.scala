package monocle

object Fixtures:

  trait Animal
  case class Tiger(stripes: Int) extends Animal
  case class Zoo(tiger: Tiger)
  case class Photo(animal: Animal)

  val tigerStripes = Optic.thatCan.edit[Tiger, Int](_.stripes)(s => _.copy(stripes = s))
  val zooTiger = Optic.thatCan.edit[Zoo, Tiger](_.tiger)(t => _.copy(tiger = t))
  val getZooTiger = Optic.thatCan.get[Zoo, Tiger](_.tiger)
  val animalPhoto = Optic.thatCan.get[Animal, Photo](Photo.apply)
  val tigerPhoto = Optic.thatCan.get[Tiger, Photo](Photo.apply)



  case class Company(name: String)
  case class Pen(color: String, manufacturer: Option[Company] = None)
  case class Office(desk: Desk, pens: List[Pen])
  case class Printer(paperJam: Boolean)
  case class Desk(numPens: Int, printer: Option[Printer])

  val companyName = Optic.thatCan.edit[Company, String](_.name)(n => _.copy(name = n))
  val getCompanyName = Optic.thatCan.get[Company, String](_.name)

  val penColor = Optic.thatCan.edit[Pen, String](_.color)(c => _.copy(color = c))
  val getPenColor = Optic.thatCan.get[Pen, String](_.color)

  val penManufacturer = Optic.thatCan.editOption[Pen, Company](_.manufacturer)(m => p => p.copy(manufacturer = p.manufacturer.map(_ => m)))
  val getPenManufacturer = Optic.thatCan.getOption[Pen, Company](_.manufacturer)

  val officeDesk = Optic.thatCan.edit[Office, Desk](_.desk)(d => _.copy(desk = d))
  val getOfficeDesk = Optic.thatCan.get[Office, Desk](_.desk)

  val officePens = Optic.thatCan.editMany[Office, Pen, List](_.pens)(pens => _.copy(pens = pens))
  val getOfficePens = Optic.thatCan.getMany[Office, Pen](_.pens)

  val deskNumPens = Optic.thatCan.edit[Desk, Int](_.numPens)(n => _.copy(numPens = n))
  val getDeskNumPens = Optic.thatCan.get[Desk, Int](_.numPens)

  val deskPrinter = Optic.thatCan.editOption[Desk, Printer](_.printer)(p => desk => desk.copy(printer = desk.printer.map(_ => p)))
  val getDeskPrinter = Optic.thatCan.getOption[Desk, Printer](_.printer)

  val printerPaperJam = Optic.thatCan.edit[Printer, Boolean](_.paperJam)(b => _.copy(paperJam = b))
  val getPrinterPaperJam = Optic.thatCan.get[Printer, Boolean](_.paperJam)

  val office = 
    Office(
      Desk(4, Some(Printer(true))), 
      List(
        Pen("red", None), 
        Pen("green", Some(Company("BobCorp"))), 
        Pen("blue")))