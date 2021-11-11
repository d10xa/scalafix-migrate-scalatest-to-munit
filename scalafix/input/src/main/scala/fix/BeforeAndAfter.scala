/*
rule = ScalafixMigrateScalatestToMunit
*/
package fix

import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuiteLike

abstract class BeforeAndAfter extends AnyFunSuiteLike with BeforeAndAfterAll {
  override protected def beforeAll(): Unit = ()
  override protected def afterAll(): Unit = ()
}
