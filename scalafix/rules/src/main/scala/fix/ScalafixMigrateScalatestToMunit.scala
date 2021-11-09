package fix

import scalafix.v1._
import scala.meta._

class ScalafixMigrateScalatestToMunit extends SemanticRule("ScalafixMigrateScalatestToMunit") {

  val replaces = Map(
    "org/scalatest/funsuite/AnyFunSuiteLike#" -> ("munit.FunSuite".parse[Importer].get, "FunSuite")
  )

  override def fix(implicit doc: SemanticDocument): Patch = {
    doc.tree.collect {
      case t@Term.ApplyInfix(leftTerm, Term.Name("shouldEqual"), List(), List(rightTerm)) =>
        val newTree = Term.Apply(
          Term.Name("assertEquals"),
          List(leftTerm, rightTerm)
        )
        Patch.replaceTree(t, newTree.toString())
      case Template(_, inits, _, _) =>
        inits.flatMap(init => replaces.get(init.symbol.value) match {
          case None =>
            List.empty
          case Some((importer, name)) =>
            List(Patch.addGlobalImport(importer), Patch.replaceTree(init, name), Patch.removeGlobalImport(init.symbol))
          case None => List.empty
        }).asPatch
    }.asPatch
  }

}
