package scala.meta
package internal
package quasiquotes

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import org.scalameta.adt.{Liftables => AdtLiftables, AdtReflection}
import org.scalameta.ast.{Liftables => AstLiftables}

// TODO: ideally, we would like to bootstrap these macros on top of scala.meta
// so that quasiquotes can be interpreted by any host, not just scalac
class Macros[C <: Context](val c: C) extends AdtReflection with AdtLiftables with AstLiftables {
  val u: c.universe.type = c.universe
  import c.universe.{Tree => _, _}
  import c.universe.{Tree => ScalaReflectTree}
  import scala.meta.{Tree => ScalaMetaTree}
  val TermQuote = "shadow scala.meta quasiquotes"
  case class Dummy(id: String, ndots: Int, arg: ScalaReflectTree)

  def apply(macroApplication: ScalaReflectTree, scalaMetaParse: String => ScalaMetaTree): ScalaReflectTree = {
    val SyntacticFlavor = symbolOf[scala.meta.syntactic.quasiquotes.Enable.type]
    val SemanticFlavor = symbolOf[scala.meta.semantic.quasiquotes.Enable.type]
    val flavor = c.inferImplicitValue(typeOf[scala.meta.quasiquotes.Flavor]).tpe.typeSymbol
    flavor match {
      case SyntacticFlavor =>
        val (skeleton, dummies) = parseSkeleton(macroApplication, scalaMetaParse)
        reifySkeleton(skeleton, dummies)
      case SemanticFlavor =>
        // TODO: this is a very naive approach to hygiene, and it will be replaced as soon as possible
        val (skeleton, dummies) = parseSkeleton(macroApplication, scalaMetaParse)
        val maybeAttributedSkeleton = scala.util.Try(attributeSkeleton(skeleton)).getOrElse(skeleton)
        reifySkeleton(maybeAttributedSkeleton, dummies)
      case _ =>
        c.abort(c.enclosingPosition, "choose the flavor of quasiquotes by importing either scala.meta.syntactic.quasiquotes._ or scala.meta.semantic.quasiquotes._")
    }
  }

  private def parseSkeleton(macroApplication: ScalaReflectTree, scalaMetaParse: String => ScalaMetaTree): (ScalaMetaTree, List[Dummy]) = {
    val q"$_($_.apply(..$partlits)).$_.apply[..$_](..$argtrees)($dialect)" = macroApplication
    val parts = partlits.map{ case q"${part: String}" => part }
    def ndots(s: String): Int = if (s.endsWith(".")) ndots(s.stripSuffix(".")) + 1 else 0
    val dummies = argtrees.zipWithIndex.map{ case (tree, i) => Dummy(c.freshName("dummy"), ndots(parts(i)), tree) }
    val snippet = (parts.init.zip(dummies).flatMap{ case (part, Dummy(id, ndots, _)) => List(if (ndots != 1) part.stripSuffix("." * ndots) else part, s"$id") } :+ parts.last).mkString("")
    (scalaMetaParse(snippet), dummies)
  }

  private def attributeSkeleton(meta: ScalaMetaTree): ScalaMetaTree = {
    val (scalaReflectParse, scalaReflectMode) = meta match {
      case _: scala.meta.Term => ((code: String) => c.parse(code), c.TERMmode)
      case _: scala.meta.Type => ((code: String) => c.parse(s"type T = $code").asInstanceOf[TypeDef].rhs, c.TYPEmode)
      case _ => sys.error("attribution of " + meta.productPrefix + " is not supported yet")
    }
    val reflect = c.typecheck(scalaReflectParse(meta.toString), mode = scalaReflectMode, silent = false)
    // TODO: correlate `meta` and `reflect` and populate denotations in `reflect`
    // afterwards `reifySkeleton` will automatically take care of reifying those denotations
    meta
  }

  private def reifySkeleton(meta: ScalaMetaTree, dummies: List[Dummy]): ScalaReflectTree = {
    implicitly[Liftable[ScalaMetaTree]].apply(meta)
  }
}