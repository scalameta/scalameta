package scala.meta.testkit

import scala.meta.Tree
object StructurallyEqual {
  /** Test if two trees are structurally equal.
    * @return Left(errorMessage with minimal diff) if trees are not structurally
    *         different, otherwise Right(Unit). To convert into exception with
    *         meaningful error message,
    *         val Right(_) = StructurallyEqual(a, b)
    **/
  def apply(a: Tree, b: Tree): Either[String, Unit] = {
    def loop(x: Any, y: Any): Either[String, Unit] = {
      val ok: Boolean = (x, y) match {
        case (x, y) if x == null || y == null => x == null && y == null
        case (x: Some[_], y: Some[_]) => loop(x.get, y.get).isRight
        case (x: None.type, y: None.type) => true
        case (xs: Seq[_], ys: Seq[_]) =>
          xs.length == ys.length &&
            xs.zip(ys).forall {
              case (x, y) => loop(x, y).isRight
            }
        case (x: Tree, y: Tree) =>
          def sameStructure =
            x.productPrefix == y.productPrefix &&
              loop(x.productIterator.toList, y.productIterator.toList).isRight
          sameStructure
        case _ => x == y
      }
      if (!ok) {
        val structure = (x, y) match {
          case (t1: Tree, t2: Tree) =>
            s"""
               |Syntax diff:
               |${t1.syntax}
               |${t2.syntax}
               |
               |Structure diff:
               |${t1.structure}
               |${t2.structure}
               |""".stripMargin
          case _ => ""
        }
        Left(s"$x != $y$structure")
      } else Right(Unit)
    }
    loop(a, b)
  }
}
