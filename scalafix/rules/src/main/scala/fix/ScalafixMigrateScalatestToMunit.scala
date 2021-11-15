package fix

import scalafix.v1._

import scala.meta._
import scala.meta.tokens.Token
import scala.meta.contrib.Whitespace

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
      case d@Defn.Def(_) if (d.name.value == "afterAll" || d.name.value == "beforeAll") =>
        d.mods.find(_.is[Mod.Protected]) match {
          case Some(value) => removeWithRedundantSpace(value.tokens.toList)
          case None => Patch.empty
        }
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
              removeWithRedundantSpace(init.tokens.toList ++ init.tokens.head.leadingKwWithOrKwExtendsWithSpaces)
            )
          case (init, Some(None)) =>
            // remove token only with `with`
            val leadingTokens = init.tokens.head.leadingKwWithOrKwExtendsWithSpaces
            leadingTokens match {
              case list if list.lastOption.exists(_.is[Token.KwWith]) =>
                List(
                  Patch.removeGlobalImport(init.symbol),
                  removeWithRedundantSpace(init.tokens.toList ++ leadingTokens)
                )
              case _ =>
                List(
                  Patch.removeGlobalImport(init.symbol),
                  removeWithRedundantSpace(init.tokens.toList)
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

  def isWhitespaceAllAround(startToken: Token, endToken: Token): Boolean =
    startToken.is[Whitespace] && endToken.is[Whitespace]

  def findRedundandWhitespaceAround(tokens: List[Token])(implicit doc: SemanticDocument): Option[Token] =
    tokens match {
      case Nil => None
      case head :: Nil =>
        for {
          leadingToken <- doc.tokenList.leading(head).headOption
          trailingToken <- doc.tokenList.trailing(head).headOption
          if isWhitespaceAllAround(leadingToken, trailingToken)
        } yield trailingToken
      case head :: next =>
        for {
          leadingToken <- doc.tokenList.leading(head).headOption
          trailingToken <- doc.tokenList.trailing(next.last).headOption
          if isWhitespaceAllAround(leadingToken, trailingToken)
        } yield trailingToken
    }

  def removeWithRedundantSpace(tokens: List[Token])(implicit doc: SemanticDocument): Patch = {
    val r = findRedundandWhitespaceAround(tokens).toList
    Patch.removeTokens(tokens ++ r)
  }
  
  implicit class TokenOps(token: Token) {
    def leadingKwWithOrKwExtendsWithSpaces(implicit doc: SemanticDocument): List[Token] =
      doc.tokenList.leading(token).toList
        .span(t => t.is[Whitespace]) match {
        case (spaces, rest) => rest.headOption match {
          case Some(t) if t.is[Token.KwWith] || t.is[Token.KwExtends] =>
            spaces :+ t
          case _ => List.empty
        }
      }
  }
}
