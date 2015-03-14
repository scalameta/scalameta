package scala.meta.interpreter.internal.test

import org.scalatest._
import scala.meta.semantic._
import scala.meta.eval._
import scala.meta.dialects.Scala211
import scala.meta._
import scala.meta.internal.hosts.scalac.contexts.StandaloneContext
import scala.reflect.{ ClassTag, classTag }
import scala.meta.internal.{ ast => m }

class RealWorldExamplesSpec extends FlatSpec with ShouldMatchers {
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
        import scala.meta.semantic.Context
        def metaprogram1(T: Type)(implicit c: Context) = {
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
              val parent = m.Term.ApplyType(m.Ctor.Ref.Name("Foo"), List(T.asInstanceOf[m.Type]))
              m.Term.New(m.Template(Nil, List(parent), m.Term.Param(Nil, m.Name.Anonymous(), None, None), Some(Nil)))
            case _ =>
              sys.error(s"unsupported type $T")
          }
        }
      }
    """)

  val m.Source(List(m.Defn.Object(_, _, _, m.Template(_, _, _, Some(List(_, _, _, _, metaprogram2: m.Defn.Def)))))) = c.define(s"""
      object DummyContainer2 {
        import scala.meta._
        import scala.meta.internal.{ast => m}
        import scala.meta.semantic.Context
        import scala.meta.dialects.Scala211
        def metaprogram2(T: Type)(implicit c: scala.meta.macros.Context) = {
          T match {
            case ref: Type.Ref =>
              val defn = ref.defn
              if (defn.isClass || defn.isTrait || defn.isObject) {
                val serializer = Term.fresh(defn.name + "Serializer")
                val input = Term.fresh("input")
                val body = {
                  def serializer(defn: Member, input: Term.Name, tagged: Boolean) = {
                    val fields = defn.ctor.params.map(x => x.field)
                    var entries: Seq[Term] = fields.map { field =>
                      """ + "q\"\"\"" + """ "\"" + ${field.name.toString} + "\": " + serialize($input.${field.name}) """ + "\"\"\"" + """
                    }
                    if (tagged) {
                      val tag = defn.parents.head.children.indexOf(defn).toString
                      entries = entries :+ """ + "q\"\"\"" + """ "$$tag: " + $tag """ + "\"\"\"" + """
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
      evalFunc(metaprogram1, List(t"List"), List(c))
    }
    ex.getMessage() should be("unsupported ref to List")
  }

  it should "reject non-final classes" in {
    val ex1 = intercept[RuntimeException] {
      evalFunc(metaprogram1, List(t"TestTraitNonFinal"), List(c))
    }
    ex1.getMessage() should be("XNonFinal is not final")
  }

  it should "reject non-case classes" in {
    val ex2 = intercept[RuntimeException] {
      evalFunc(metaprogram1, List(t"TestTraitNonCase"), List(c))
    }
    ex2.getMessage() should be("XNonCase is not a case class")
  }

  it should "verify the sealed case class hierarchies" in {
    val res = evalFunc(metaprogram1, List(t"TestTrait"), List(c))
    res.asInstanceOf[m.Tree].show[Code] should be("new Foo[TestTrait] {}")
  }

  it should "verify the case objects" in {
    val res = evalFunc(metaprogram1, List(t"SerObject.type"), List(c))
    res.asInstanceOf[m.Tree].show[Code] should be("new Foo[SerObject.type] {}")
  }

  it should "verify the case classes" in {
    val res = evalFunc(metaprogram1, List(t"TestCaseClass"), List(c))
    res.asInstanceOf[m.Tree].show[Code] should be("new Foo[TestCaseClass] {}")
  }

  "Synthesis macro" should "produce a materializer" in {
    val res = evalFunc(metaprogram2, List(t"TestTrait"), List(c))
    res.asInstanceOf[m.Tree].show[Code] should be("""{
      |  implicit object TestTraitSerializer15 extends Serializer[TestTrait] {
      |    def apply(input16: TestTrait): String = input16 match {
      |      case input17: X => "{" + ("$tag: " + "0") + "}"
      |      case input17: Y => "{" + ("$tag: " + "1") + "}"
      |    }
      |  }
      |  TestTraitSerializer15
      |}""".stripMargin)

  }

  it should "generate a serializer for objects" in {
    val res = evalFunc(metaprogram2, List(t"SerObject.type"), List(c))
    res.asInstanceOf[m.Tree].show[Code] should be("""{
      |  implicit object SerObjectSerializer20 extends Serializer[SerObject.type] { def apply(input21: SerObject.type): String = "{" + "" + "}" }
      |  SerObjectSerializer20
      |}""".stripMargin)
  }

  it should "generate a serializer for case classes" in {
    val res = evalFunc(metaprogram2, List(t"TestCaseClass"), List(c))
    res.asInstanceOf[m.Tree].show[Code] should be("""{
      |  implicit object TestCaseClassSerializer24 extends Serializer[TestCaseClass] { def apply(input25: TestCaseClass): String = "{" + "" + "}" }
      |  TestCaseClassSerializer24
      |}""".stripMargin)
  }

}
