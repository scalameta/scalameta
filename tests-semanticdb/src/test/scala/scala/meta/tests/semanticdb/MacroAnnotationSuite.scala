package scala.meta.tests.semanticdb

// NOTE(olafur): when this test suite is merged with TargetedSuite you get the following warning:
//   package ap  @org.scalameta.data.data class A(a: In... *** FAILED ***
//   <input>:3:2: error macro implementation not found: macroTransform
//   (the most common reason for that is that you cannot use macro implementations in the same compilation run that defines them)
//   @org.scalameta.data.data
//    ^ (SemanticdbSuite.scala:70)
// I am unable to reproduce the error when running this test individually with `-- -z "package ap"`
// or by moving the test to a separate suite.
class MacroAnnotationSuite extends SemanticdbSuite {

  targeted(
    """|package a
       |
       |@org.scalameta.data.data
       |class A(a: Int)
       |""".stripMargin,
    { doc =>
      val symbols = doc.symbols.map(_.symbol).sorted
      assert(symbols.contains("a/A#"))
      assert(symbols.contains("a/A#productElement()."))
      assert(symbols.contains("a/A.unapply()."))
      assert(symbols.contains("a/A.apply()."))
    },
  )

  occurrences(
    """|package ma1
       |@org.scalameta.data.data
       |class A(a: Int)
       |object o { val x = A(1); x.a; val A(y) = x }
       |""".stripMargin,
    """|[0:8..0:11): ma1 <= ma1/
       |[1:1..1:4): org => org/
       |[1:5..1:14): scalameta => org/scalameta/
       |[1:15..1:19): data => org/scalameta/data/
       |[1:20..1:24): data => org/scalameta/data/data#
       |[2:6..2:7): A <= ma1/A#
       |[2:7..2:7):  <= ma1/A#`<init>`().
       |[2:11..2:14): Int => scala/Int#
       |[3:7..3:8): o <= ma1/o.
       |[3:15..3:16): x <= ma1/o.x.
       |[3:19..3:20): A => ma1/A.
       |[3:25..3:26): x => ma1/o.x.
       |[3:27..3:28): a => ma1/A#a.
       |[3:34..3:35): A => ma1/A.
       |[3:36..3:37): y <= ma1/o.y.
       |[3:41..3:42): x => ma1/o.x.
       |""".stripMargin,
  )

}
