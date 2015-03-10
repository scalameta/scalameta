package scala.meta.interpreter.internal.test

import org.scalatest._
import scala.meta.semantic._
import scala.meta.internal.interpreter.Interpreter
import scala.meta.dialects.Scala211
import scala.meta._
import scala.meta.internal.{ ast => impl }
import scala.meta.internal.hosts.scalac.contexts.StandaloneContext
import scala.meta.internal.hosts.scalac.Scalahost
import scala.reflect.{ ClassTag, classTag }
import scala.meta.internal.{ ast => i }

class RealWorldExamplesSpec extends FlatSpec with ShouldMatchers {

  def jarOf[T: ClassTag] = classTag[T].runtimeClass.getProtectionDomain().getCodeSource().getLocation().getFile()
  val cp = List(jarOf[org.scalameta.UnreachableError], jarOf[scala.meta.Tree]).mkString(java.io.File.pathSeparator)
  implicit val c = Scalahost.mkStandaloneContext(s"-cp $cp:" + System.getProperty("sbt.paths.tests.classpath"))
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
                if (!leaf.isCase) sys.error(s"${leaf.name} is not a case class")
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

  val impl.Source(List(impl.Defn.Object(_, _, _, impl.Template(_, _, _, Some(List(_, _, _, _, metaprogram2)))))) = c.define(s"""
      object DummyContainer {
        import scala.meta._
        import scala.meta.internal.{ast => impl}
        import scala.meta.semantic.Context
        import scala.meta.dialects.Scala211
        def metaprogram(T: Type)(implicit c: scala.meta.macros.Context) = {
          T match {
            case ref: Type.Ref =>
              val defn = ref.defn
              if (defn.isClass || defn.isTrait || defn.isObject) {
                val serializer = Term.fresh(defn.name + "Serializer")
                val input = Term.fresh("input")
                val body = {
                  def serializer(defn: Member, input: Term.Name, tagged: Boolean) = {
                    val fields = defn.ctor.params.map(_.field)
                    var entries: Seq[Term] = fields.map { field =>
                      """ + "q\"\"\"" + """ "\"" + ${field.name.toString} + "\": " + serialize($input.${field.name}) """ + "\"\"\"" + """
                    }
                    if (tagged) {
                      val tag = defn.parents.head.children.indexOf(defn).toString
                      entries :+= """ + "q\"\"\"" + """ "$$tag: " + $tag """ + "\"\"\"" + """
                    }
                    val unwrappedResult = entries.foldLeft(None: Option[Term]) { (acc, curr) =>
                      acc.map(acc => """ + "q\"\"\"" + """$acc + ", " + $curr""" + "\"\"\"" + """).orElse(Some(curr))
                    }
                    val contents = unwrappedResult.getOrElse(""" + "q\"\"\"" + """ "" """ + "\"\"\"" + """)
                    """ + "q\"\"\"" + """ "{" + $contents + "}" """ + "\"\"\"" + """
                  }
                  if (defn.isClass) {
                    serializer(defn, input, tagged = false)
                  } else if (defn.isObject) {
                    serializer(defn, input, tagged = false)
                  } else if (defn.isTrait) {
                    val refined = Pat.fresh("input")
                    val clauses = defn.children.map(leaf => p"case $refined: ${leaf.tpe.pat} => ${serializer(leaf, refined.name, tagged = true)}")
                    q"$input match { ..$clauses }"
                  } else {
                    abort(s"unsupported ref to ${defn.name}")
                  }
                }
                """ + "q\"\"\"" + """
                  implicit object $serializer extends Serializer[$T] {
                    def apply($input: $T): String = $body
                  }
                  $serializer
                """ + "\"\"\"" + """
              } else {
                abort(s"unsupported ref to ${defn.name}")
              }
            case _ =>
              abort(s"unsupported type $T")
          }
        }
      }
    """)

  "A verification macro" should "reject defns that are not classes, traits or objects" in {
    val ex = intercept[RuntimeException] {
      Interpreter.evalFunc(metaprogram, List(t"List"), List(c))
    }
    ex.getMessage() should be("unsupported ref to List")
  }

  it should "reject non-final classes" in {
    val ex1 = intercept[RuntimeException] {
      Interpreter.evalFunc(metaprogram, List(t"TestTraitNonFinal"), List(c))
    }
    ex1.getMessage() should be("XNonFinal is not final")
  }

  it should "reject non-case classes" in {
    val ex2 = intercept[RuntimeException] {
      Interpreter.evalFunc(metaprogram, List(t"TestTraitNonCase"), List(c))
    }
    ex2.getMessage() should be("XNonCase is not a case class")
  }

  it should "evaluate the sealed case classes" in {
    val res = Interpreter.evalFunc(metaprogram, List(t"TestTrait"), List(c))
    res.asInstanceOf[scala.meta.internal.interpreter.environment.Object].ref.asInstanceOf[i.Tree].show[Code] should be("new Foo[TestTrait] {}")
  }

  // TODO failing test
  //  "Synthesis macro" should "produce a materializer" in {
  //    Interpreter.evalFunc(metaprogram2, List(t"TestTrait"), List(c))
  //  }

}
