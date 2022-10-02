package fix

class ShouldStartWith extends AnyFunSuiteLikeWithMatchers {
  test("should startWith") {
    val str = "ab"
    assert("abc".startsWith(str))
  }
}
