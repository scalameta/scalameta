package scala.meta
package syntactic.quasiquotes

import scala.reflect.macros.whitebox.Context
import org.scalameta.adt._
import scala.meta.syntactic.parsers.ParseSyntaxError

class Macros[C <: Context](val c: C) extends AdtReflection with NewLiftables {
  val u: c.universe.type = c.universe
  import c.universe.{Tree => _, _}
  import c.universe.{Tree => ScalaTree}
  import scala.meta.{Tree => PalladiumTree}
  def apply(macroApplication: ScalaTree, parse: String => PalladiumTree): ScalaTree = {
    val TermQuote = "denied" // TODO: find a cleaner way out of this mess
    val q"$_($_.apply(..$partlits)).$_.apply[..$_](..$argtrees)" = macroApplication
    val parts = partlits.map{ case q"${part: String}" => part }
    val args = argtrees.map(tree => c.freshName(tree.toString) -> tree)
    val snippet = (parts.init.zip(args).flatMap{ case (part, (id, _)) => List(part, s"`$id`") } :+ parts.last).mkString("")
    val liveTree: PalladiumTree = parse(snippet)
    val reifiedTree: ScalaTree = implicitly[Liftable[PalladiumTree]].apply(liveTree)
    reifiedTree
  }
}
