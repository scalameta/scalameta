package scala.meta
package internal
package semantic

import org.scalameta.adt
import org.scalameta.adt._
import org.scalameta.invariants._

@monadicRoot trait Typing
object Typing {
  @noneLeaf object Zero extends Typing
  @noneLeaf object Recursive extends Typing
  @someLeaf class Nonrecursive(tpe: Type.Arg @byNeed) extends Typing {
    protected def writeReplace(): AnyRef = new Nonrecursive.SerializationProxy(this)
  }
  object Nonrecursive {
    @SerialVersionUID(1L) private class SerializationProxy(@transient private var orig: Nonrecursive) extends Serializable {
      private def writeObject(out: java.io.ObjectOutputStream): Unit = {
        out.writeObject(orig.tpe)
      }
      private def readObject(in: java.io.ObjectInputStream): Unit = {
        val tpe = in.readObject.asInstanceOf[Type.Arg]
        orig = Nonrecursive(tpe)
      }
      private def readResolve(): AnyRef = orig
      override def toString = s"Proxy($orig)"
    }
  }
}

// TODO: This unrelated code is here because of the limitation of knownDirectSubclasses.
// We would like move it to scala/meta/internal/quasiquotes/ast/ReificationMacros.scala where it belongs,
// but then we have problems with compilation order.
trait TypingLiftables extends adt.Liftables {
  implicit def liftableSubTree[T <: Tree]: u.Liftable[T]
  lazy implicit val liftableTyping: u.Liftable[Typing] = materializeAdt[Typing]
}
