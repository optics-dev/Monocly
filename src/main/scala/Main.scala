import example.Foo.{FooI, FooIS}
import example.{Mono, Poly, Foo}
import optics.PPrism

object Main {

  def main(args: Array[String]): Unit = {
    val mono = Mono(1, FooIS(8, "hey"), Some(0))
    println((Mono.foo >>> Foo.fooIS).getOption(mono))
    println((Mono.foo >>> (Foo.fooIS >>> FooIS.i)).getOption(mono))
    println((Mono.optI >>> PPrism.some).getOption(mono))

    val poly1 = Poly(1, FooIS(8, "hey"), Some(0), Option(3))
    // change a Poly[Option[Int]] to  Poly[Option[String]]
    println((Poly.param >>> PPrism.some).set("hello")(poly1))

    val poly2 = Poly(1, FooIS(8, "hey"), Some(0), Some(3))
    // change a Poly[Some[Int]] to  Poly[Some[String]]
    println((Poly.param >>> PPrism.some).set("hello")(poly1))

    // combine poly and monomorphic optics
    val poly3 = Poly(1, FooIS(8, "hey"), Some(0), Some(FooI(4)))
    println((Poly.param >>> PPrism.some >>> Foo.fooI).set(FooI(7))(poly3))
  }

}
