package scala.meta.interpreter.internal.test

import org.scalatest._
import scala.meta.semantic._
import scala.meta.internal.interpreter.Interpreter
import scala.meta.dialects.Scala211
import scala.meta._
import scala.meta.ui._
import scala.meta.internal.{ ast => impl }
import scala.meta.internal.hosts.scalac.contexts.StandaloneContext

class RealWorldExamplesSpec extends FlatSpec with ShouldMatchers {

  /*def jarOf[T: ClassTag] = classTag[T].runtimeClass.getProtectionDomain().getCodeSource().getLocation().getFile()
  val cp = List(jarOf[org.scalameta.UnreachableError], jarOf[scala.meta.Tree]).mkString(java.io.File.pathSeparator)
  implicit val c = Scalahost.mkStandaloneContext(s"-cp $cp")
  val impl.Source(List(impl.Defn.Object(_, _, _, impl.Template(_, _, _, Some(List(_, _, _, metaprogram)))))) = c.define("""
      object DummyContainer {
        import scala.meta._
        import scala.meta.internal.{ast => impl}
        import scala.meta.semantic.Context
        def metaprogram(T: Type)(implicit c: Context) = {
          T match {
            case ref: Type.Ref =>
              def validateLeaf(leaf: Member) = {
                if (!leaf.isFinal) sys.error(s"${leaf.name} is not final")
                if (!leaf.isCase) sys.error(s"${leaf.name} is not sealed")
                if (!leaf.tparams.isEmpty) sys.error(s"${leaf.name} is not monomorphic")
              }
              val defn = ref.defn
              if (defn.isClass || defn.isObject) {
                validateLeaf(defn)
              } else if (defn.isTrait) {
                if (defn.isSealed) defn.children.foreach(validateLeaf)
                else sys.error(s"${defn.name} is not sealed")
              } else {
                sys.error(s"unsupported ref to ${defn.name}")
              }
              val parent = impl.Term.ApplyType(impl.Ctor.Ref.Name("Foo"), List(T.asInstanceOf[impl.Type]))
              impl.Term.New(impl.Template(Nil, List(parent), impl.Term.Param(Nil, impl.Name.Anonymous(), None, None), Some(Nil)))
            case _ =>
              sys.error(s"unsupported type $T")
          }
        }
      }
    """)

  def evalFunc(metaprogram: impl.Tree, args: Seq[Any]*): Any = {
    ???
  }

  "An interpreter" should "execute macros" in {
    evalFunc(metaprogram, List(t"Int"), List(c))
  }*/

}
