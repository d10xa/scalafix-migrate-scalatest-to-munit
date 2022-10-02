/*
rule = ScalafixMigrateScalatestToMunit
*/
package fix

class BeEmpty extends AnyFunSuiteLikeWithMatchers {
  test("should be empty") {
    val a = ""
    a should be(empty)
  }
}

