/*
rule = ScalafixMigrateScalatestToMunit
*/
package fix

class ShouldStartWith extends AnyFunSuiteLikeWithMatchers {
  test("should startWith") {
    val str = "ab"
    "abc" should startWith(str)
  }
}

