import example.Foo.{FooI, FooIS, FooList, FooS}
import example.{Foo, Mono, Poly}
import optics.mono.{NonEmptyTraversal, Prism, Traversal}
import optics.poly.{NonEmptyPTraversal, PPrism}

object Main {

  def main(args: Array[String]): Unit = {

    val monoSome = Mono(1, FooIS(8, "hey"), Some(0))
    val monoNone = Mono(1, FooIS(8, "hey"), None)

    println((Mono.foo >>> Foo.fooIS).getOption(monoSome))
    println((Mono.foo >>> Foo.fooIS >>> FooIS.i).getOption(monoSome))
    println((Mono.optI >>> Prism.some).getOption(monoSome))
    // return error message
    println((Mono.optI >>> Prism.some).getOrError(monoNone))


    val poly1 = Poly(1, FooIS(8, "hey"), Some(0), Option(3))
    // change a Poly[Option[Int]] to  Poly[Option[String]]
    println((Poly.pparam >>> PPrism.some).replaceOrError("hello")(poly1))

    val poly2 = Poly(1, FooIS(8, "hey"), Some(0), Some(3))
    // change a Poly[Some[Int]] to  Poly[Some[String]]
    println((Poly.pparam >>> PPrism.some).replaceOrError("hello")(poly2))

    // combine poly and monomorphic optics
    val poly3 = Poly(1, FooIS(8, "hey"), Some(0), Option(FooI(4): Foo))
    println((Poly.param >>> Prism.some >>> Foo.fooI).replaceOrError(FooI(7))(poly3))

    // Left(Expected FooIS but got FooI(4))
//    println((Poly.param >>> PPrism.some >>> Foo.fooIS).getOrError(poly3))
    // Left(it is none)
//    println((Poly.param >>> PPrism.some >>> Foo.fooIS).getOrError(poly3.copy(param = None)))


    // (true,false)
    println(NonEmptyPTraversal.pair.modify((_: Int) % 2 == 0)((10, 7)))
    // List(1, 3, 6, 10)
    println((NonEmptyPTraversal.pair >>> NonEmptyPTraversal.pair).toListOrError(((1, 3), (6, 10))))
    // List(1, 3, 6, 10)
    println((NonEmptyTraversal.pair >>> NonEmptyTraversal.pair).toListOrError(((1, 3), (6, 10))))

    // Left(Expected FooI but got FooS(hello))
    println((Traversal.list >>>  Foo.fooI).toListOrError(List(FooI(10), FooS("hello"), FooI(0), FooIS(3, "world"))))
    // Left(List is empty)x
    println((Traversal.list >>>  Foo.fooI).toListOrError(Nil))
    // Right(List(FooI(10), FooI(0)))
//    println((PTraversal.list >>>? Foo.fooI).toListOrError(List(FooI(10), FooS("hello"), FooI(0), FooIS(3, "world"))))

    // Right(List(1, 2, 3, 4))
    println((Foo.fooL >>>  Traversal.list).toListOrError(FooList(List(1,2,3,4))))
    // Left(List is empty)
    println((Foo.fooL >>>  Traversal.list).toListOrError(FooList(Nil)))
    // Left(Expected FooList but got FooI(10))
    println((Foo.fooL >>>  Traversal.list).toListOrError(FooI(10)))
    // Right(List()) TODO It should be a Left if there is no item matching
//    println((Foo.fooL ?>>> PTraversal.list).toListOrError(FooI(10)))

  }

}
