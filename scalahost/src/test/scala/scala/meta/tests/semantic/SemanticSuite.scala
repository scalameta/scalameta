package scala.meta.tests
package semantic

import org.scalatest._
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.internal.{ast => m}
import scala.meta.dialects.Scala211

class SemanticSuite extends FunSuite {
  implicit val c = Context(Artifact(sys.props("sbt.paths.scalalibrary.classes")))

  test("easy 1") {
    assert(t"List[Int]".show[Semantics] === """
      |Type.Apply(Type.Name("List")[1], Seq(Type.Name("Int")[2]))
      |[1] {1}::scala.package#List
      |[2] {2}::scala#Int
      |[3] {2}::scala.package
      |[4] {3}::scala
      |[5] {0}::_root_
      |{1} Type.Singleton(Term.Name("package")[3]{1}<>)
      |{2} Type.Singleton(Term.Name("scala")[4]{2}<>)
      |{3} Type.Singleton(Term.Name("_root_")[5]{3}<>)
    """.trim.stripMargin)
  }

  test("easy 2") {
    def actual(s: String): String = {
      val guidRegex = "[0-9a-fA-F]{8}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{4}-[0-9a-fA-F]{12}".r
      guidRegex.replaceAllIn(s, "<GUID>")
    }

    def expected(s: String): String = {
      def loop(s: String): String = {
        val s1 = s.replace("  ", " ")
        if (s == s1) s1 else loop(s1)
      }
      loop(s.trim.stripMargin.replace("\\\n", "")).replace("( ", "(").replace(" )", ")")
    }

    assert(actual(q"""
      object Main {
        def main(args: Array[String]): Unit = {
          val x = 2 + 2
          val y = x * x
          println(y)
        }
      }
    """.show[Semantics]) === expected("""
      |Defn.Object(\
      |  Nil, Term.Name("Main")[1]{1}<>,\
      |  Template(Nil, Nil,\
      |    Term.Param(Nil, Name.Anonymous()[2], None, None){1},\
      |    Some(Seq(\
      |      Defn.Def(\
      |        Nil, Term.Name("main")[3]{2}<>, Nil,\
      |        Seq(Seq(\
      |          Term.Param(\
      |            Nil, Term.Name("args")[4]{3}<>,\
      |            Some(Type.Apply(Type.Name("Array")[5], Seq(Type.Name("String")[6]))), None){3})),\
      |        Some(Type.Name("Unit")[7]),\
      |        Term.Block(Seq(\
      |          Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("x")[8]{4}<>)), None, Term.ApplyInfix(Lit(2){4}<>, Term.Name("+")[9]{5}<>, Nil, Seq(Lit(2){4}<>)){4}<1>),\
      |          Defn.Val(Nil, Seq(Pat.Var.Term(Term.Name("y")[10]{4}<>)), None, Term.ApplyInfix(Term.Name("x")[8]{4}<>, Term.Name("*")[11]{6}<>, Nil, Seq(Term.Name("x")[8]{4}<>)){4}<>),\
      |          Term.Apply(Term.Name("println")[12]{7}<2>, Seq(Term.Name("y")[10]{4}<>)){8}<>)){8}<>)))))
      |[1] {0}::local#<GUID>
      |[2] {0}::local#<GUID>
      |[3] {1}::local#<GUID>
      |[4] {0}::local#<GUID>
      |[5] {9}::scala#Array
      |[6] {10}::scala.Predef#String
      |[7] {9}::scala#Unit
      |[8] {0}::local#<GUID>
      |[9] {4}::scala#Int.+(I)I
      |[10] {0}::local#<GUID>
      |[11] {11}::scala#Int.*(I)I
      |[12] {10}::scala.Predef.println(Ljava/lang/Object;)V
      |[13] {13}::scala
      |[14] {9}::scala.Predef
      |[15] {9}::scala#Int
      |[16] {0}::scala#Int.+(I)I.x
      |[17] {0}::scala#Int.*(I)I.x
      |[18] {0}::scala.Predef.println(Ljava/lang/Object;)V.x
      |[19] {9}::scala#Any
      |[20] {0}::_root_
      |{1} Type.Singleton(Term.Name("Main")[1]{1}<>)
      |{2} Type.Method(Seq(Seq(Term.Param(Nil, Term.Name("args")[4]{3}<>, Some(Type.Apply(Type.Name("Array")[5], Seq(Type.Name("String")[6]))), None){3})), Type.Name("Unit")[7])
      |{3} Type.Apply(Type.Name("Array")[5], Seq(Type.Name("String")[6]))
      |{4} Type.Name("Int")[15]
      |{5} Type.Method(Seq(Seq(Term.Param(Nil, Term.Name("x")[16]{4}<>, Some(Type.Name("Int")[15]), None){4})), Type.Name("Int")[15])
      |{6} Type.Method(Seq(Seq(Term.Param(Nil, Term.Name("x")[17]{4}<>, Some(Type.Name("Int")[15]), None){4})), Type.Name("Int")[15])
      |{7} Type.Method(Seq(Seq(Term.Param(Nil, Term.Name("x")[18]{12}<>, Some(Type.Name("Any")[19]), None){12})), Type.Name("Unit")[7])
      |{8} Type.Name("Unit")[7]
      |{9} Type.Singleton(Term.Name("scala")[13]{9}<>)
      |{10} Type.Singleton(Term.Name("Predef")[14]{10}<>)
      |{11} Type.Singleton(Term.Name("x")[8]{4}<>)
      |{12} Type.Name("Any")[19]
      |{13} Type.Singleton(Term.Name("_root_")[20]{13}<>)
      |<1> Term.Apply(Term.Select(Lit(2){4}<>, Term.Name("+")[9]{5}<>){5}<>, Seq(Lit(2){4}<>)){4}<>
      |<2> Term.Select(Term.Select(Term.This(Name.Indeterminate("scala")[13]){9}<>, Term.Name("Predef")[14]{10}<>){10}<>, Term.Name("println")[12]{7}<>){7}<>
    """))
  }
}