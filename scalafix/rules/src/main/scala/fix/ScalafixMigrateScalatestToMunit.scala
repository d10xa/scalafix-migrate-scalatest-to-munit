package fix

import scalafix.v1._

import scala.meta._
import scala.meta.tokens.Token

class ScalafixMigrateScalatestToMunit extends SemanticRule("ScalafixMigrateScalatestToMunit") {

  val ScalatestAnyFunSuiteLike_S: Symbol = Symbol("org/scalatest/funsuite/AnyFunSuiteLike#")
  val ScalatestBeforeAndAfterAll_S: Symbol = Symbol("org/scalatest/BeforeAndAfterAll#")
  val MunitFunSuite_S: Symbol = Symbol("munit/FunSuite#")

  val replaces: Map[Symbol, Option[Symbol]] = Map(
    ScalatestAnyFunSuiteLike_S -> Some(MunitFunSuite_S),
    ScalatestBeforeAndAfterAll_S -> None
  )

  override def fix(implicit doc: SemanticDocument): Patch = {
    doc.tree.collect {
      case t@Term.ApplyInfix(leftTerm, Term.Name("shouldEqual"), List(), List(rightTerm)) =>
        val newTree = Term.Apply(
          Term.Name("assertEquals"),
          List(leftTerm, rightTerm)
        )
        Patch.replaceTree(t, newTree.toString())
      case Template(_, inits, _, _) if inits.nonEmpty =>
        val initsWithReplaces = inits.map(i => (i, replaces.get(i.symbol)))
        val needRemoveExtends = initsWithReplaces.forall {
          case (_, Some(None)) => true
          case _ => false
        }
        initsWithReplaces.flatMap {
          case (init, None) => List.empty
          case (init, Some(None)) if needRemoveExtends =>
            // remove token with `extends` and `with`
            List(
              Patch.removeGlobalImport(init.symbol),
              Patch.removeTokens(init.tokens.toList ::: init.tokens.head.leadingKwWithOrKwExtendsWithSpaces)
            )
          case (init, Some(None)) =>
            // remove token only with `with`
            val leadingTokens = init.tokens.head.leadingKwWithOrKwExtendsWithSpaces
            leadingTokens match {
              case list if list.lastOption.exists(_.is[Token.KwWith]) =>
                List(
                  Patch.removeGlobalImport(init.symbol),
                  Patch.removeTokens(init.tokens.toList ::: leadingTokens)
                )
              case _ =>
                List(
                  Patch.removeGlobalImport(init.symbol),
                  Patch.removeTokens(init.tokens)
                )
            }
          case (init, Some(Some(symbol))) =>
            // replace
            List(
              Patch.addGlobalImport(symbol),
              Patch.removeGlobalImport(init.symbol),
              Patch.replaceTree(init, symbol.displayName)
            )
        }.asPatch
    }.asPatch
  }

  implicit class TokenOps(token: Token) {
    def leadingKwWithOrKwExtendsWithSpaces(implicit doc: SemanticDocument): List[Token] =
      doc.tokenList.leading(token).toList
        .span(t => t.is[Token.Space] || t.is[Token.LF]) match {
        case (spaces, rest) => rest.headOption match {
          case Some(t) if t.is[Token.KwWith] || t.is[Token.KwExtends] =>
            spaces :+ t
          case _ => List.empty
        }
      }
  }
}
