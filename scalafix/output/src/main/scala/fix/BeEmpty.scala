package fix

class BeEmpty extends AnyFunSuiteLikeWithMatchers {
  test("should be empty") {
    val a = ""
    assert(a.isEmpty)
  }
}
