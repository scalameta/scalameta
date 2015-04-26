package scala.meta.interpreter.internal.test

import org.scalatest._
import scala.meta.semantic._
import scala.meta.eval._
import scala.meta.dialects.Scala211
import scala.meta._
import scala.meta.internal.hosts.scalac.contexts.StandaloneContext
import scala.reflect.{ ClassTag, classTag }
import scala.meta.internal.{ ast => m }
import scala.reflect.macros.runtime.AbortMacroException

class NewRealWorldExamplesSpec extends FlatSpec with ShouldMatchers {
  def evalFunc(defn: m.Defn.Def, argss: Seq[Any]*)(implicit c: semantic.Context): Any = {
    val env = scala.collection.mutable.Map[Term.Name, Any]()
    val evalee = argss.foldLeft(defn.name: Term)((curr, args) => {
      val margs = args.map(arg => { val name = Term.fresh("arg"); env(name) = arg; name })
      m.Term.Apply(curr.asInstanceOf[m.Term], margs.asInstanceOf[scala.collection.immutable.Seq[m.Term]])
    })
    evalee.eval(env.toMap)
  }

  def jarOf[T: ClassTag] = classTag[T].runtimeClass.getProtectionDomain().getCodeSource().getLocation().getFile()
  val cp = List(jarOf[org.scalameta.UnreachableError], jarOf[scala.meta.Tree]).mkString(java.io.File.pathSeparator)
  implicit val c = Scalahost.mkStandaloneContext(s"-cp $cp:" + System.getProperty("sbt.paths.tests.classpath"))
  val m.Source(List(m.Defn.Object(_, _, _, m.Template(_, _, _, Some(List(_, _, _, metaprogram1: m.Defn.Def)))))) = c.define("""
      object DummyContainer1 {
        import scala.meta._
        import scala.meta.internal.{ast => m}
        import scala.meta.dialects.Scala211
        def metaprogram1(T: Type)(implicit c: scala.meta.macros.Context) = {
          T match {
            case ref: Type.Ref =>
              def validate(defn: Member): Unit = {
                def isEffectivelyFinal(defn: Member): Boolean = {
                  if (defn.isFinal) true
                  else if (defn.isSealed) defn.children.forall(x => x.isFinal)
                  else false
                }
                if (!isEffectivelyFinal(defn)) abort(s"${defn.name} is not a final class/object or a sealed parent of final classes/objects")
                val cases = (defn +: defn.children).filter(x => x.isFinal)
                cases.filter(x => !x.isCase).foreach(x => abort(s"${x.name} is not a case class or a case object"))
                cases.filter(x => !x.tparams.isEmpty).foreach(x => abort(s"${x.name} is not monomorphic"))
              }
              validate(ref.defn)
              q"new _root_.adt.Adt[$T]{}"
            case _ =>
              abort(s"unsupported type $T")
          }
        }
      }
    """)

  val m.Source(List(m.Defn.Object(_, _, _, m.Template(_, _, _, Some(List(_, _, _, metaprogram2: m.Defn.Def)))))) = c.define("""
      object DummyContainer2 {
        import scala.meta._
        import scala.meta.internal.{ast => m}
        import scala.meta.dialects.Scala211
        def metaprogram2(T: Type)(implicit c: scala.meta.macros.Context) = {
          T match {
            case ref: Type.Ref =>
              def serializerFor(defn: Member, x: Term.Name, tagged: Boolean): Term = {
                if (defn.isFinal) {
                  val serializedTag = {
                    if (tagged) {
                      val tag = defn.parents.head.children.indexOf(defn).toString
                      qQQQ${"$tag"} + ": " + $tagQQQ
                    } else {
                      qQQQ "" QQQ
                    }
                  }
                  val fields = defn.ctor.params.map(x => x.field)
                  val serializedFields = fields.map(f => qQQQ${f.name.toString} + ": " + _root_.serialization.serialize($x.${f.name})QQQ)
                  val payload = serializedFields.foldLeft(serializedTag)((acc, curr) => qQQQ$acc + ", " + $currQQQ)
                  qQQQ "{ " + $payload + " }" QQQ
                } else {
                  val cases = defn.children.map(child => {
                    val x = Pat.fresh("x")
                    p"case $x: ${child.tpe.pat} => ${serializerFor(child, x.name, tagged = true)}"
                  })
                  q"$x match { ..$cases }"
                }
              }
              val x = Term.fresh("x")
              val name = Term.fresh("Serializer")
              val body = serializerFor(ref.defn, x, tagged = false)
              qQQQ
                implicit object $name extends _root_.serialization.Serializer[$T] { 
                  def apply($x: $T): _root_.scala.Predef.String = $body
                }
                $name
              QQQ
            case _ =>
              abort(s"unsupported type $T")
          }
        }
      }
    """.replace("QQQ", "\"\"\""))

  "A verification macro" should "reject defns that are not classes, traits or objects" in {
    val ex = intercept[AbortMacroException] {
      evalFunc(metaprogram1, List(t"List"), List(c))
    }
    ex.getMessage() should be("List is not a final class/object or a sealed parent of final classes/objects")
  }

  it should "reject non-final classes" in {
    val ex1 = intercept[AbortMacroException] {
      evalFunc(metaprogram1, List(t"TestTraitNonFinal"), List(c))
    }
    ex1.getMessage() should be("TestTraitNonFinal is not a final class/object or a sealed parent of final classes/objects")
  }

  it should "reject non-case classes" in {
    val ex2 = intercept[AbortMacroException] {
      evalFunc(metaprogram1, List(t"TestTraitNonCase"), List(c))
    }
    ex2.getMessage() should be("XNonCase is not a case class or a case object")
  }

  it should "verify the sealed case class hierarchies" in {
    val res = evalFunc(metaprogram1, List(t"TestTrait"), List(c))
    res.asInstanceOf[m.Tree].show[Code] should be("new _root_.adt.Adt[TestTrait] {}")
  }

  it should "verify the case objects" in {
    val res = evalFunc(metaprogram1, List(t"SerObject.type"), List(c))
    res.asInstanceOf[m.Tree].show[Code] should be("new _root_.adt.Adt[SerObject.type] {}")
  }

  it should "verify the case classes" in {
    val res = evalFunc(metaprogram1, List(t"TestCaseClass"), List(c))
    res.asInstanceOf[m.Tree].show[Code] should be("new _root_.adt.Adt[TestCaseClass] {}")
  }

  "Synthesis macro" should "produce a materializer" in {
    val res = evalFunc(metaprogram2, List(t"TestTrait"), List(c))
    res.asInstanceOf[m.Tree].show[Code] should be("""{
      |  implicit object Serializer41 extends _root_.serialization.Serializer[TestTrait] {
      |    def apply(x40: TestTrait): _root_.scala.Predef.String = x40 match {
      |      case x42: X => "{ " + ("$tag" + ": " + "0") + " }"
      |      case x43: Y => "{ " + ("$tag" + ": " + "1") + " }"
      |    }
      |  }
      |  Serializer41
      |}""".stripMargin)

  }

  it should "generate a serializer for objects" in {
    val res = evalFunc(metaprogram2, List(t"SerObject.type"), List(c))
    res.asInstanceOf[m.Tree].show[Code] should be("""{
      |  implicit object Serializer47 extends _root_.serialization.Serializer[SerObject.type] { def apply(x46: SerObject.type): _root_.scala.Predef.String = "{ " + "" + " }" }
      |  Serializer47
      |}""".stripMargin)
  }

  it should "generate a serializer for case classes" in {
    val res = evalFunc(metaprogram2, List(t"TestCaseClass"), List(c))
    res.asInstanceOf[m.Tree].show[Code] should be("""{
      |  implicit object Serializer51 extends _root_.serialization.Serializer[TestCaseClass] { def apply(x50: TestCaseClass): _root_.scala.Predef.String = "{ " + "" + " }" }
      |  Serializer51
      |}""".stripMargin)
  }

}
