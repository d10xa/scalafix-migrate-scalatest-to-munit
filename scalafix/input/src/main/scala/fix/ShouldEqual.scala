/*
rule = ScalafixMigrateScalatestToMunit
*/
package fix
class ShouldEqual extends AnyFunSuiteLikeWithMatchers {
  test("shouldEqual") {
    val a = 1
    val b = 2
    // comment
    a + b shouldEqual b + a
  }
}
