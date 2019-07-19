import example.Foo.FooIS
import example.{Bar, Foo, Id}
import optics.{Lens, Prism}

object Main {

  val bar = Bar(1, Id("xxx"), FooIS(8, "hey"), Some(0))

  def main(args: Array[String]): Unit = {
    println((Bar.id compose Id.x).get(bar))
    println((Bar.id compose (Id.x: Lens[Id, String])).get(bar))
    println((Bar.foo compose Foo.fooIS).getOption(bar))
    println((Bar.foo compose (Foo.fooIS compose FooIS.i)).getOption(bar))

    def some[A]: Prism[Option[A], A] = Prism[Option[A], A](identity)(Some(_))

    println((Bar.optI compose some).getOption(bar))
  }




}
