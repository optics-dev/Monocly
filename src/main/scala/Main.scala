import example.Foo.{FooI, FooIS, FooList, FooS}
import example.{Foo, Mono, Poly}
import optics.{EPPrism, PPrism, PTraversal}

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
    println((Poly.param >>> some).replaceOrError("hello")(poly1))

    val poly2 = Poly(1, FooIS(8, "hey"), Some(0), Some(3))
    // change a Poly[Some[Int]] to  Poly[Some[String]]
    println((Poly.param >>> some).replaceOrError("hello")(poly2))

    // combine poly and monomorphic optics
    val poly3 = Poly(1, FooIS(8, "hey"), Some(0), Some(FooI(4)))
    println((Poly.param >>> some >>> Foo.fooI).replaceOrError(FooI(7))(poly3))

    // Left(Expected FooIS but got FooI(4))
    println((Poly.param >>> some >>> Foo.fooIS).getOrError(poly3))
    // Left(it is none)
    println((Poly.param >>> some >>> Foo.fooIS).getOrError(poly3.copy(param = None)))


    // (true,false)
    println(PTraversal.pair.modify((_: Int) % 2 == 0)((10, 7)))
    // List(1, 3, 6, 10)
    println((PTraversal.pair >>> PTraversal.pair).toList(((1, 3), (6, 10))))

    // Right(List(FooI(10), FooI(4)))
    println((PTraversal.list >>>  Foo.fooI).toListOrError(List(FooI(10), FooI(4))))
    // Left(Expected FooI but got FooS(hello))
    println((PTraversal.list >>>  Foo.fooI).toListOrError(List(FooI(10), FooS("hello"), FooI(0), FooIS(3, "world"))))
    // Right(List(FooI(10), FooI(0)))
    println((PTraversal.list >>>? Foo.fooI).toListOrError(List(FooI(10), FooS("hello"), FooI(0), FooIS(3, "world"))))

    // Right(List(1, 2, 3, 4))
    println((Foo.fooL >>>  PTraversal.list).toListOrError(FooList(List(1,2,3,4))))
    // Left(Expected FooList but got FooI(10))
    println((Foo.fooL >>>  PTraversal.list).toListOrError(FooI(10)))
    // Right(List()) TODO It should be a Left if there is no item matching
    println((Foo.fooL ?>>> PTraversal.list).toListOrError(FooI(10)))

  }

}
