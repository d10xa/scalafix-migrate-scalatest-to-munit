package fix

import munit.FunSuite

abstract class BeforeAndAfter extends FunSuite  {
  override protected def beforeAll(): Unit = ()
  override protected def afterAll(): Unit = ()
}
