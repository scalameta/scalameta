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
    """.stripMargin, { doc =>
      val symbols = doc.symbols.map(_.symbol).sorted
      assert(symbols.contains("a/A#"))
      assert(symbols.contains("a/A#productElement()."))
      assert(symbols.contains("a/A.unapply()."))
      assert(symbols.contains("a/A.apply()."))
    }
  )

}
