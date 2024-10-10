package scala.meta.tests
package parsers

import org.scalameta.internal.ScalaCompat.EOL
import scala.meta._
import scala.meta.dialects.Sbt0137

class SbtSuite extends TreeSuiteBase {
  test("\"...\".parse[Source]") {
    val tree = simpleBuildSyntax.parse[Source].get
    assertStruct(tree)(simpleBuildStructure)
    assertSyntaxWithClue(tree)(expectedSyntax)(simpleBuildStructure)
  }

  test("\"...\".parse[Stat]")(intercept[ParseException](simpleBuildSyntax.parse[Stat].get))

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
    assertStruct(tree)(simpleBuildStructure)
    assertSyntaxWithClue(tree)(expectedSyntax)(simpleBuildStructure)
  }

  private def simpleBuildSyntax = """|
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
                                     |""".stripMargin.lf2nl
  // native fails with a positive lookahead
  private def expectedSyntax: String = simpleBuildSyntax.replaceAll(s"[,]$EOL[ ]+", ", ")
    .replaceAll(s"$EOL[ ]+", "").replaceAll(s"$EOL[)]", ")").replaceAll(s"$EOL$EOL", EOL)

  private def simpleBuildStructure = // all use the same here
    """|Source(List(
       |  Defn.Val(
       |    List(
       |      Mod.Lazy()
       |    ),
       |    List(
       |      Pat.Var(Term.Name("commonSettings"))
       |    ),
       |    None,
       |    Term.Apply(
       |      Term.Name("Seq"),
       |      Term.ArgClause(List(
       |        Term.ApplyInfix(
       |          Term.Name("organization"),
       |          Term.Name(":="),
       |          Type.ArgClause(Nil),
       |          Term.ArgClause(List(
       |            Lit.String("com.example")
       |          ))
       |        ),
       |        Term.ApplyInfix(
       |          Term.Name("version"),
       |          Term.Name(":="),
       |          Type.ArgClause(Nil),
       |          Term.ArgClause(List(
       |            Lit.String("0.1.0")
       |          ))
       |        ),
       |        Term.ApplyInfix(
       |          Term.Name("scalaVersion"),
       |          Term.Name(":="),
       |          Type.ArgClause(Nil),
       |          Term.ArgClause(List(
       |            Lit.String("2.11.7")
       |          ))
       |        )
       |      ))
       |    )
       |  ),
       |  Defn.Val(
       |    List(
       |      Mod.Lazy()
       |    ),
       |    List(
       |      Pat.Var(Term.Name("root"))
       |    ),
       |    None,
       |    Term.Apply(
       |      Term.Select(
       |        Term.Apply(
       |          Term.Select(
       |            Term.ApplyInfix(
       |              Term.Name("project"),
       |              Term.Name("in"),
       |              Type.ArgClause(Nil),
       |              Term.ArgClause(List(
       |                Term.Apply(
       |                  Term.Name("file"),
       |                  Term.ArgClause(List(
       |                    Lit.String(".")
       |                  ))
       |                )
       |              ))
       |            ),
       |            Term.Name("settings")
       |          ),
       |          Term.ArgClause(List(
       |            Term.Repeated(Term.Name("commonSettings"))
       |          ))
       |        ),
       |        Term.Name("settings")
       |      ),
       |      Term.ArgClause(List(
       |        Term.ApplyInfix(
       |          Term.Name("name"),
       |          Term.Name(":="),
       |          Type.ArgClause(Nil),
       |          Term.ArgClause(List(
       |            Lit.String("hello")
       |          ))
       |        )
       |      ))
       |    )
       |  )
       |))
       |""".stripMargin
}
