package monocly.impl

import munit.FunSuite

class GetterImplTest extends munit.FunSuite {

  test("GetMany getOne") {
    val getter = GetManyImpl((x: List[Int]) => x)
    val input = List(1,2,3)
    assertEquals(
      getter.get(input),
      1
    )
  }

  test("GetMany getOne doesn't compile") {
    assertNoDiff(
      compileErrors("GetManyImpl((x: List[Int]) => x).get(List(1,2,3))"),
      "Error: ..."
    )
  }

  test("NoGetter getOne doesn't compile") {
    assertNoDiff(
      compileErrors("NoGetter.get(1)"),
      "Error: ..."
    )
  }

}
