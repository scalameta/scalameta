package scala.meta.tests
package parsers

import munit._
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.dialects.Sbt0137

class SbtSuite extends FunSuite {
  test("\"...\".parse[Source]") {
    val tree = simpleBuildSyntax.parse[Source].get
    assertEquals(tree.syntax, simpleBuildSyntax)
    assertEquals(tree.structure, simpleBuildStructure)
  }

  test("\"...\".parse[Stat]") {
    intercept[ParseException] { simpleBuildSyntax.parse[Stat].get }
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
    assertEquals(
      tree.syntax,
      """
      |lazy val commonSettings = Seq(organization := "com.example", version := "0.1.0", scalaVersion := "2.11.7")
      |lazy val root = (project in file(".")).settings(commonSettings: _*).settings(name := "hello")
    """.trim.stripMargin.split('\n').mkString(EOL)
    )
    assertEquals(tree.structure, simpleBuildStructure)
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
  """.trim.stripMargin.split('\n').mkString(EOL)

  private def simpleBuildStructure = {
    """
      |Source(List(
      |  Defn.Val(
      |    List(Mod.Lazy()),
      |    List(Pat.Var(Term.Name("commonSettings"))),
      |    None,
      |    Term.Apply(
      |      Term.Name("Seq"),
      |      List(
      |        Term.ApplyInfix(
      |          Term.Name("organization"),
      |          Term.Name(":="),
      |          Type.ArgClause(Nil),
      |          List(Lit.String("com.example"))
      |        ),
      |        Term.ApplyInfix(
      |          Term.Name("version"),
      |          Term.Name(":="),
      |          Type.ArgClause(Nil),
      |          List(Lit.String("0.1.0"))
      |        ),
      |        Term.ApplyInfix(
      |          Term.Name("scalaVersion"),
      |          Term.Name(":="),
      |          Type.ArgClause(Nil),
      |          List(Lit.String("2.11.7"))
      |        )
      |      )
      |    )
      |  ),
      |  Defn.Val(
      |    List(Mod.Lazy()),
      |    List(Pat.Var(Term.Name("root"))),
      |    None,
      |    Term.Apply(
      |      Term.Select(
      |        Term.Apply(
      |          Term.Select(
      |            Term.ApplyInfix(
      |              Term.Name("project"),
      |              Term.Name("in"),
      |              Type.ArgClause(Nil),
      |              List(Term.Apply(Term.Name("file"), List(Lit.String("."))))
      |            ),
      |            Term.Name("settings")
      |          ),
      |          List(Term.Repeated(Term.Name("commonSettings")))
      |        ),
      |        Term.Name("settings")
      |      ),
      |      List(
      |        Term.ApplyInfix(
      |          Term.Name("name"),
      |          Term.Name(":="),
      |          Type.ArgClause(Nil),
      |          List(Lit.String("hello"))
      |        )
      |      )
      |    )
      |  )
      |))
      |""".stripMargin.trim.replaceAll("[,]\n *", ", ").replaceAll("\n *", "")
  }
}
