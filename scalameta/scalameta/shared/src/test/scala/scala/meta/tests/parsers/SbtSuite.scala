package scala.meta.tests
package parsers

import org.scalatest._
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.dialects.Sbt0137

class SbtSuite extends FunSuite {
  test("\"...\".parse[Source]") {
    val tree = simpleBuildSyntax.parse[Source].get
    assert(tree.show[Syntax] === simpleBuildSyntax)
    assert(tree.show[Structure] === simpleBuildStructure)
  }

  test("\"...\".parse[Stat]") {
    intercept[ParseException]{ simpleBuildSyntax.parse[Stat].get }
  }

  test("source\"...\"") {
    val tree = source"""
      lazy val commonSettings = Seq(
        organization := "com.example",
        version := "0.1.0",
        scalaVersion := "2.11.7"
      )

      lazy val root = (project in file(".")).
        settings(commonSettings: _*).
        settings(
          name := "hello"
        )
    """
    // NOTE: not checking against simpleBuildSyntax because quasiquotes don't retain tokens
    assert(tree.show[Syntax] === """
      |lazy val commonSettings = Seq(organization := "com.example", version := "0.1.0", scalaVersion := "2.11.7")
      |lazy val root = (project in file(".")).settings(commonSettings: _*).settings(name := "hello")
    """.trim.stripMargin)
    assert(tree.show[Structure] === simpleBuildStructure)
  }

  private def simpleBuildSyntax = """
    |lazy val commonSettings = Seq(
    |  organization := "com.example",
    |  version := "0.1.0",
    |  scalaVersion := "2.11.7"
    |)
    |
    |lazy val root = (project in file(".")).
    |  settings(commonSettings: _*).
    |  settings(
    |    name := "hello"
    |  )
  """.trim.stripMargin

  private def simpleBuildStructure = {
    """
      |Source(Seq(
        |Defn.Val(
          |Seq(Mod.Lazy()),
          | Seq(Pat.Var.Term(Term.Name("commonSettings"))),
          | None,
          | Term.Apply(
              |Term.Name("Seq"),
              | Seq(
                  |Term.ApplyInfix(Term.Name("organization"), Term.Name(":="), Nil, Seq(Lit("com.example"))),
                  | Term.ApplyInfix(Term.Name("version"), Term.Name(":="), Nil, Seq(Lit("0.1.0"))),
                  | Term.ApplyInfix(Term.Name("scalaVersion"), Term.Name(":="), Nil, Seq(Lit("2.11.7")))))),
        | Defn.Val(
          |Seq(Mod.Lazy()),
          | Seq(Pat.Var.Term(Term.Name("root"))),
          | None,
          | Term.Apply(
              |Term.Select(
                |Term.Apply(
                  |Term.Select(Term.ApplyInfix(Term.Name("project"), Term.Name("in"), Nil, Seq(Term.Apply(Term.Name("file"), Seq(Lit("."))))), Term.Name("settings")),
                  | Seq(Term.Arg.Repeated(Term.Name("commonSettings")))),
                | Term.Name("settings")),
              | Seq(
                  |Term.ApplyInfix(Term.Name("name"), Term.Name(":="), Nil, Seq(Lit("hello"))))))))
    """.trim.stripMargin.split(EOL).mkString("")
  }
}