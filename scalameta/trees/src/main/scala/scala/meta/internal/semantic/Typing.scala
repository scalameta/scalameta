package scala.meta
package internal
package semantic

import org.scalameta.adt
import org.scalameta.adt._
import org.scalameta.invariants._
import scala.meta.common._
import scala.meta.prettyprinters._
import scala.meta.internal.prettyprinters._

@root trait Typing extends Optional
object Typing {
  @none object None extends Typing
  @leaf object Recursive extends Typing
  @leaf class Nonrecursive(tpe: Type.Arg @byNeed) extends Typing {
    protected def writeReplace(): AnyRef = new Nonrecursive.SerializationProxy(this)
    override def canEqual(other: Any): Boolean = other.isInstanceOf[Nonrecursive]
    override def equals(that: Any): Boolean = that match {
      case that: Nonrecursive => equality.Semantic.equals(this.tpe, that.tpe)
      case _ => false
    }
    override def hashCode: Int = equality.Semantic.hashCode(tpe)
  }
  object Nonrecursive {
    def apply(tpe: => Type.Arg): Typing = new Nonrecursive(() => tpe.setTypechecked)

    @SerialVersionUID(1L) private class SerializationProxy(@transient private var orig: Nonrecursive) extends Serializable {
      private def writeObject(out: java.io.ObjectOutputStream): Unit = {
        out.writeObject(orig.tpe)
      }
      private def readObject(in: java.io.ObjectInputStream): Unit = {
        val tpe = in.readObject.asInstanceOf[Type.Arg]
        orig = new Nonrecursive(() => tpe)
        val _ = orig.tpe
        require(orig.isTpeLoaded)
      }
      private def readResolve(): AnyRef = orig
      override def toString = s"Proxy($orig)"
    }
  }
}

// NOTE: Need this code in this very file in order to avoid issues with knownDirectSubclasses.
// Without this, compilation order may unexpectedly affect compilation success.
trait TypingLiftables extends adt.Liftables {
  implicit def liftableSubTree[T <: Tree]: u.Liftable[T]
  lazy implicit val liftableTyping: u.Liftable[Typing] = materializeAdt[Typing]
}
