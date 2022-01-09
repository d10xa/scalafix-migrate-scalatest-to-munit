package fix

class ShouldEqualMethodInvocation extends AnyFunSuiteLikeWithMatchers {
  test(".shouldEqual") {
    val a = 1
    val b = 2
    // comment
    assertEquals(a + b, b + a)
  }
}
