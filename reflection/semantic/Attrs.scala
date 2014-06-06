package scala.reflect
package semantic

import org.scalareflect.adt._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.reflect.core._

@root trait Attr
object Attr {
  @leaf class Defn(defn: Tree) extends Attr
  @leaf class Type(tpe: Param.Type) extends Attr
  @leaf class InferredTargs(targs: Seq[Type]) extends Attr
  @leaf class InferredVargs(vargs: Seq[Term]) extends Attr
  @leaf class MacroExpansion(tree: Tree) extends Attr
}
