/*
rule = ScalafixMigrateScalatestToMunit
*/
package fix
import org.scalatest.funsuite.AnyFunSuiteLike
import org.scalatest.matchers.should.Matchers

abstract class AnyFunSuiteLikeWithMatchers
  extends AnyFunSuiteLike
    with Matchers {}
