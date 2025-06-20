package scala.meta.tests

import scala.meta.Dialect
import scala.meta.Token
import scala.meta._

object TestHelpers {

  def getMessageWithExtraClue(msg: String, extraClue: String = ""): String =
    if (extraClue.isEmpty) msg else s"$msg ($extraClue)"

  def getSyntax(code: => String, syntax: String = null): String = Option(syntax).getOrElse(code)

  trait Tokenize {
    def apply(code: String)(implicit dialect: Dialect): Iterable[Token]
  }

  def tokensAsStructureLines(tokens: Iterator[Token]) = tokens.map(_.structure).mkString("\n")

  def tokensAsSyntax(tokens: Iterator[Token]) = tokens.map(_.syntax).mkString

  def collect[B](tree: Tree)(pf: PartialFunction[Tree, B]): List[B] = {
    val builder = List.newBuilder[B]
    def loop(t: Tree): Unit = {
      builder ++= pf.lift(t)
      t.children.foreach(loop)
    }
    loop(tree)
    builder.result()
  }

}
