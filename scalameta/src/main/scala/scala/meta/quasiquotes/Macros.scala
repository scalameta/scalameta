package scala.meta
package internal
package quasiquotes

import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context
import org.scalameta.adt._

// TODO: ideally, we would like to bootstrap these macros on top of scala.meta
// so that quasiquotes can be interpreted by any host, not just scalac
class Macros[C <: Context](val c: C) extends AdtReflection with NewLiftables {
  val u: c.universe.type = c.universe
  import c.universe.{Tree => _, _}
  import c.universe.{Tree => ScalaReflectTree}
  import scala.meta.{Tree => ScalaMetaTree}
  def apply(macroApplication: ScalaReflectTree, parse: String => ScalaMetaTree): ScalaReflectTree = {
    val SyntacticFlavor = symbolOf[scala.meta.syntactic.quasiquotes.Enable.type]
    val SemanticFlavor = symbolOf[scala.meta.semantic.quasiquotes.Enable.type]
    val flavor = c.inferImplicitValue(typeOf[scala.meta.quasiquotes.Flavor]).tpe.typeSymbol
    flavor match {
      case SyntacticFlavor =>
        val TermQuote = "denied" // TODO: find a cleaner way out of this mess
        val q"$_($_.apply(..$partlits)).$_.apply[..$_](..$argtrees)($dialect)" = macroApplication
        val parts = partlits.map{ case q"${part: String}" => part }
        def ndots(s: String): Int = if (s.endsWith(".")) ndots(s.stripSuffix(".")) + 1 else 0
        val args = argtrees.zipWithIndex.map{ case (tree, i) => (c.freshName("dummy"), ndots(parts(i)), tree) }
        val snippet = (parts.init.zip(args).flatMap{ case (part, (id, ndots, _)) => List(if (ndots != 1) part.stripSuffix("." * ndots) else part, s"$id") } :+ parts.last).mkString("")
        val liveTree: ScalaMetaTree = parse(snippet)
        val reifiedTree: ScalaReflectTree = implicitly[Liftable[ScalaMetaTree]].apply(liveTree)
        reifiedTree
      case SemanticFlavor =>
        // TODO: pre-typecheck the quasiquote and pre-populate denotations, so that we have at least a glimpse of hygiene
        // this is a very naive approach to hygiene, and it will be replaced as soon as possible
        c.abort(c.enclosingPosition, "semantic quasiquotes are not yet implemented")
      case _ =>
        c.abort(c.enclosingPosition, "choose the flavor of quasiquotes by importing either `syntactic.quasiquotes._` or `semantic.quasiquotes._`")
    }
  }
}
