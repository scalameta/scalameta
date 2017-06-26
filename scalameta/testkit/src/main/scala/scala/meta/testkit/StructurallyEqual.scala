package scala.meta.testkit

import scala.meta.Tree

object StructurallyEqual {

  /** Test if two trees are structurally equal.
    * @return Left(errorMessage with minimal diff) if trees are not structurally
    *         different, otherwise Right(Unit). To convert into exception with
    *         meaningful error message,
    *         val Right(_) = StructurallyEqual(a, b)
    **/
  def apply(a: Tree, b: Tree): Either[AnyDiff, Unit] = {
    def loop(x: Any, y: Any): Boolean = {
      val ok: Boolean = (x, y) match {
        case (x, y) if x == null || y == null => x == null && y == null
        case (x: Some[_], y: Some[_]) => loop(x.get, y.get)
        case (x: None.type, y: None.type) => true
        case (xs: List[_], ys: List[_]) =>
          xs.length == ys.length &&
            xs.zip(ys).forall {
              case (x, y) => loop(x, y)
            }
        case (x: Tree, y: Tree) =>
          def sameStructure =
            x.productPrefix == y.productPrefix &&
              loop(x.productIterator.toList, y.productIterator.toList)
          sameStructure
        case _ => x == y
      }
      if (!ok) throw AnyDiff(x, y)
      else true
    }
    try {
      loop(a, b)
      Right(Unit)
    } catch {
      case t: AnyDiff => Left(t)
    }
  }
}
