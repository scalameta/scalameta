package scala.meta.tests.semanticdb

import scala.meta.internal.{semanticdb => s}

// https://github.com/scalameta/scalameta/issues/1830
class Issue1830Suite extends SemanticdbSuite {

  private def typeParams(doc: s.TextDocument): List[String] = doc.symbols.map(_.symbol)
    .filter(_.endsWith("]")).toList

  // 2.12 rejects two anonymous type params of one class, method or type alias
  if (ScalaVersion.is213) {
    targeted(
      """|package w1
         |object X { type T[_, _] = Int }
         |""".stripMargin,
      doc => assertEquals(typeParams(doc), List("w1/X.T#[_$$1]", "w1/X.T#[_$$2]")),
    )

    targeted(
      """|package w2
         |class C[_, _]
         |""".stripMargin,
      doc => assertEquals(typeParams(doc), List("w2/C#[_$$1]", "w2/C#[_$$2]")),
    )

    targeted(
      """|package w3
         |object X { def m[_, _] = ??? }
         |""".stripMargin,
      doc => assertEquals(typeParams(doc), List("w3/X.m().[_$$1]", "w3/X.m().[_$$2]")),
    )

    targeted(
      """|package w4
         |class C[A, _, B, _]
         |""".stripMargin,
      doc => assertEquals(typeParams(doc), List("w4/C#[A]", "w4/C#[B]", "w4/C#[_$$1]", "w4/C#[_$$2]")),
    )
  }

  targeted(
    """|package w5
       |trait X { type T[_, _] }
       |""".stripMargin,
    doc =>
      assertEquals(
        typeParams(doc),
        if (ScalaVersion.is212) List("w5/X#T#[_]") else List("w5/X#T#[_$$1]", "w5/X#T#[_$$2]"),
      ),
  )

  targeted(
    """|package w6
       |object X { def m[F[_, _]] = ??? }
       |""".stripMargin,
    doc =>
      assertEquals(
        typeParams(doc),
        if (ScalaVersion.is212) List("w6/X.m().[F]", "w6/X.m().[F][_]")
        else List("w6/X.m().[F]", "w6/X.m().[F][_$$1]", "w6/X.m().[F][_$$2]"),
      ),
  )

  targeted(
    """|package w7
       |class C[F[_]] { def m[_] = ??? }
       |""".stripMargin,
    doc =>
      assertEquals(
        typeParams(doc),
        if (ScalaVersion.is212) List("w7/C#[F]", "w7/C#[F][_]", "w7/C#m().[_]")
        else List("w7/C#[F]", "w7/C#[F][_$$1]", "w7/C#m().[_$$2]"),
      ),
  )

}
