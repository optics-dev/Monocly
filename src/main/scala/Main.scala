import example.Foo.{FooI, FooIS}
import example.{Foo, Mono, Poly}
import optics.{EPPrism, PPrism}

object Main {

  def main(args: Array[String]): Unit = {
    def some[A, B] = EPPrism.some[String, A, B]("it is none")

    val monoSome = Mono(1, FooIS(8, "hey"), Some(0))
    val monoNone = Mono(1, FooIS(8, "hey"), None)

    println((Mono.foo >>> Foo.fooIS).getOption(monoSome))
    println((Mono.foo >>> Foo.fooIS >>> FooIS.i).getOption(monoSome))
    println((Mono.optI >>> some).getOption(monoSome))
    // return error message
    println((Mono.optI >>> some).getOrError(monoNone))


    val poly1 = Poly(1, FooIS(8, "hey"), Some(0), Option(3))
    // change a Poly[Option[Int]] to  Poly[Option[String]]
    println((Poly.param >>> some).set("hello")(poly1))

    val poly2 = Poly(1, FooIS(8, "hey"), Some(0), Some(3))
    // change a Poly[Some[Int]] to  Poly[Some[String]]
    println((Poly.param >>> some).set("hello")(poly2))

    // combine poly and monomorphic optics
    val poly3 = Poly(1, FooIS(8, "hey"), Some(0), Some(FooI(4)))
    println((Poly.param >>> some >>> Foo.fooI).set(FooI(7))(poly3))

    // Left(Expected FooIS but got FooI(4))
    println((Poly.param >>> some >>> Foo.fooIS).getOrError(poly3))
    // Left(it is none)
    println((Poly.param >>> some >>> Foo.fooIS).getOrError(poly3.copy(param = None)))

  }

}
