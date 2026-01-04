package scala.meta.tests

import scala.meta._

import scala.annotation.tailrec

object TestHelpers {

  def getMessageWithExtraClue(msg: String, extraClue: String = ""): String =
    if (extraClue.isEmpty) msg else s"$msg ($extraClue)"

  def getSyntax(code: => String, syntax: String = null): String = Option(syntax).getOrElse(code)

  trait Tokenize {
    def apply(code: String)(implicit dialect: Dialect): Iterable[Token]
  }

  def tokensAsStructureLines(tokens: Iterator[Token]) = tokens.map(_.structure).mkString("\n")

  def tokensAsSyntax(tokens: Iterator[Token]) = tokens.map(_.syntax).mkString

  def foreach(tree: Tree)(pf: PartialFunction[Tree, Unit]): Unit = {
    @tailrec
    def iter(treess: List[List[Tree]]): Unit = treess match {
      case trees :: treess => trees match {
          case tree :: trees =>
            pf.applyOrElse(tree, (_: Tree) => ())
            iter(tree.children :: trees :: treess)
          case _ => iter(treess)
        }
      case _ =>
    }
    iter(List(List(tree)))
  }

  def collect[B](tree: Tree)(pf: PartialFunction[Tree, B]): List[B] = {
    val builder = List.newBuilder[B]
    foreach(tree)(pf.andThen(builder += _))
    builder.result()
  }

}
