package fix

import munit.FunSuite

abstract class BeforeAndAfter extends FunSuite  {
  override def beforeAll(): Unit = ()
  override def afterAll(): Unit = ()
}
