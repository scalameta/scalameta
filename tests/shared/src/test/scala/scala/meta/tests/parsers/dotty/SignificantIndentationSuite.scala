package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class SignificantIndentationSuite extends BaseDottySuite {
  implicit val parseStat: String => Stat = code => templStat(code)(dialects.Dotty)
  implicit val parseSource: String => Source = code => source(code)(dialects.Dotty)

  val defx = Decl.Def(Nil, Term.Name("f"), Nil, Nil, Type.Name("Int"))
  val defy = Defn.Def(Nil, Term.Name("y"), Nil, Nil, Some(Type.Name("String")), Term.Block(List(Term.Apply(Term.Name("fa"), Nil), Term.Apply(Term.Name("fb"), Nil))))

  test("basic-example") {
    val code = """|trait A:
                  |  def f: Int
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = Some("trait A { def f: Int }"))(
      Defn.Trait(Nil, Type.Name("A"), Nil, Ctor.Primary(Nil, Name(""), Nil), Template(Nil, Nil, Nil, Self(Name(""), None), List(
        defx)))
    )
  }

  test("multistat-example") {
    val code = """|trait A:
                  |  def f: Int
                  |  def y: String = { fa(); fb() }
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Defn.Trait(Nil, Type.Name("A"), Nil, Ctor.Primary(Nil, Name(""), Nil), Template(Nil, Nil, Nil, Self(Name(""), None), List(
        defx, defy
        )))
    )
  }

  test("indent-and-back") {
    val code = """|object O:
                  |  class C:
                  |    def f: Int = 3
                  |  trait T:
                  |    def f: Int = 4
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
Defn.Object(Nil, Term.Name("O"), Template(Nil, Nil, Nil, Self(Name(""), None), List(
  Defn.Class(Nil, Type.Name("C"), Nil, ctor, Template(Nil, Nil, Nil, Self(Name(""), None), List(
    Defn.Def(Nil, Term.Name("f"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(3))))),
   Defn.Trait(Nil, Type.Name("T"), Nil, Ctor.Primary(Nil, Name(""), Nil), Template(Nil, Nil, Nil, Self(Name(""), None), List(
     Defn.Def(Nil, Term.Name("f"), Nil, Nil, Some(Type.Name("Int")), Lit.Int(4)))))
   )))
    )
  }
  
  test("indent-match-two") {
    val code = """|
                  |x match
                  |  case 1 => "OK"
                  |  case 2 => "ERROR"
                  |""".stripMargin
    runTestAssert[Stat](code, assertLayout = None)(
      Term.Match(Term.Name("x"), List(Case(Lit.Int(1), None, Lit.String("OK")), Case(Lit.Int(2), None, Lit.String("ERROR"))))
    )
  }
}
