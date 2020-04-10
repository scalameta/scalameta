package scala.meta.tests.parsers

import scala.meta.Source
import scala.meta.Dialect
import scala.meta._

class GivenUsingSuite extends ParseSuite {
  /** For checking examples in repl declare:
   *  trait Ord[T] { def f(): Int }
   * 
   *  All examples based on dotty documentation:
   *  https://dotty.epfl.ch/docs/reference/contextual/givens.html
   *  https://dotty.epfl.ch/docs/reference/contextual/using-clauses.html
   *  https://dotty.epfl.ch/docs/reference/contextual/given-imports.html
   */

  //TODO: CHECK ALL STATEMENTS COMPILE IN DOTTY here in tests!

  // ---------------------------------
  // GIVEN
  // ---------------------------------

  test("given-named") {
    runTest("given intOrd as Ord[Int] { def f(): Int = 1 }") { stat =>
      val Defn.Given(Nil, meta.Name("intOrd"), Nil, Nil, Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
        Template(Nil, Nil, Self(Name(""), None), List(Defn.Def(Nil, Term.Name("f"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(1))))
      ) = stat
    }
  }

  test("given-named-newline") {
    runTest("given intOrd as Ord[Int] \n { def f(): Int = 1 }") { stat =>
      val Defn.Given(Nil, meta.Name("intOrd"), Nil, Nil, Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
        Template(Nil, Nil, Self(Name(""), None), List(Defn.Def(Nil, Term.Name("f"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(1))))
      ) = stat
    }
  }

  test("given-anonymous") {
    runTest("given Ord[Int] { def f(): Int = 1 }") { stat =>
      val Defn.Given(Nil, meta.Name.Anonymous(), Nil, Nil, Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
        _) = stat
    }
  }

  test("given-override-def") {
    runTest("given intOrd as Ord[Int] { override def f(): Int = 1 }") { stat =>
      val Defn.Given(Nil, meta.Name("intOrd"), Nil, Nil, Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
        Template(Nil, Nil, Self(Name(""), None), List(Defn.Def(List(Mod.Override()), Term.Name("f"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(1)
      )))) = stat
    }
  }

  test("given-self") {
    runTest("given intOrd as Ord[Int] { current => }") { stat =>
      val Defn.Given(Nil, meta.Name("intOrd"), Nil, Nil, Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
        Template(Nil, Nil, Self(Name("current"), None), List())
      ) = stat
    }
  }

  test("given-selftype-error".ignore) {
    runTestError("given intOrd as Ord[Int] { current: Ord[Int] => }", "objects must not have a self type")
  }

  test("given-no-block") {
    runTest("given intOrd as Ord[Int]") { stat =>
      val Defn.Given(Nil, meta.Name("intOrd"), Nil, Nil, Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
        Template(Nil, Nil, Self(Name(""), None), List())
      ) = stat
    }
  }

  test("given-anonymous-no-block") {
    runTest("given Ord[Int]") { stat =>
      val Defn.Given(Nil, meta.Name.Anonymous(), Nil, Nil, Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
        Template(Nil, Nil, Self(Name(""), None), List())
      ) = stat
    }
  }

  test("given-generic-named") {
    runTest("given listOrd[T] as Ord[List[T]] { def f(): Int = 1 }") { stat =>
    val Defn.Given(Nil, meta.Name("listOrd"), List(Type.Param(Nil, Type.Name("T"), Nil, _, Nil, Nil)), Nil,
      Type.Apply(Type.Name("Ord"), List(Type.Apply(Type.Name("List"), List(Type.Name("T"))))),
        Template(Nil, Nil, _, List(Defn.Def(Nil, Term.Name("f"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(1))))
        ) = stat
    }
  }

  test("given-generic-anonymous") {
    runTest("given Ord[List[T]] { def f(): Int = 1 }") { stat =>
      val Defn.Given(Nil, meta.Name.Anonymous(), Nil, Nil, Type.Apply(Type.Name("Ord"),
       List(Type.Apply(Type.Name("List"), List(Type.Name("T"))))),
         Template(Nil, Nil, _, List(Defn.Def(Nil, Term.Name("f"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(1))))
      ) = stat
    }
  }

  test("given-depend-given-named") {
    runTest("given setOrd[T](using ord: Ord[T]) as Ord[Set[T]]") { stat =>
      val Defn.Given(Nil, meta.Name("setOrd"), List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        List(List(Term.Param(List(Mod.Using()), Term.Name("ord"), Some(Type.Apply(Type.Name("Ord"), List(Type.Name("T")))), None))),
        Type.Apply(Type.Name("Ord"), List(Type.Apply(Type.Name("Set"), List(Type.Name("T"))))),
        Template(Nil, Nil, _, List())
      ) = stat
    }
  }

  test("given-depend-given-anonymous") {
    runTest("given [T](using ord: Ord[T]) as Ord[Set[T]]") { stat =>
      val Defn.Given(Nil, meta.Name.Anonymous(), List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        List(List(Term.Param(List(Mod.Using()), Term.Name("ord"), Some(Type.Apply(Type.Name("Ord"), List(Type.Name("T")))), None))),
        Type.Apply(Type.Name("Ord"), List(Type.Apply(Type.Name("Set"), List(Type.Name("T"))))),
        Template(Nil, Nil, _, List())
      ) = stat
    }
  }

  test("given-depend-given-anonymous-using") {
    runTest("given (using Ord[String]) as Ord[Int]") { stat =>
      val Defn.Given(Nil, Name(""), Nil, List(List(Term.Param(List(Mod.Using()), Name(""), Some(Type.Apply(Type.Name("Ord"),
        List(Type.Name("String")))), None))), Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
         Template(_, _, _, _)
      ) = stat
    }

  }

  test("given-inline") {
    runTest("inline given intOrd as Ord[Int] { def f(): Int = 1 }") { stat =>
      val Defn.Given(List(Mod.Inline()), meta.Name("intOrd"), Nil, Nil, Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
        Template(Nil, Nil, _, _)
      ) = stat
    }
  }

  test("given-subtype-error".ignore) {
    // it is treaten as alias without '=' sign at the end and {...} is refinement part
    runTestError("given intOrd as _ <: Ord[Int] { def f(): Int = 1 }", "missing = at the end")
  }

  // ---------------------------------
  // GIVEN ALIAS
  // ---------------------------------

  test("given-alias-named") {
    runTest("given global as Option[Int] = Some(3)") { stat =>
      val Defn.GivenAlias(Nil, meta.Name("global"), Nil, Nil, Type.Apply(Type.Name("Option"), List(Type.Name("Int"))),
        Term.Apply(Term.Name("Some"), List(Lit.Int(3)))
      ) = stat
    }
  }

  test("given-alias-anonymous") {
    runTest("given Option[Int] = Some(3)") { stat =>
      val Defn.GivenAlias(Nil, meta.Name.Anonymous(), Nil, Nil, Type.Apply(Type.Name("Option"), List(Type.Name("Int"))),
        Term.Apply(Term.Name("Some"), List(Lit.Int(3)))
      ) = stat
    }

  }

  test("given-alias-block") {
    runTest("given global as Option[Int] = { def fx(): Int = 3; Some(3) }") { stat =>
      val Defn.GivenAlias(Nil, Type.Name("global"), Nil, Nil, Type.Apply(Type.Name("Option"), List(Type.Name("Int"))),
       Term.Block(List(Defn.Def(Nil, Term.Name("fx"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(3)),
        Term.Apply(Term.Name("Some"), List(Lit.Int(3)))))
         ) = stat

    }
  }

  test("given-alias-override-block-error".ignore) {
    runTestError("given global as Option[Int] = { override def fx(): Int = 3; Some(3) }", "no modifier allowed here")
  }

  test("given-alias-using-named") {
    runTest("given ordInt(using ord: Ord[Int]) as Ord[List[Int]] = ???") { stat =>
      val Defn.GivenAlias(Nil, meta.Name("ordInt"), Nil, List(List(Term.Param(List(Mod.Using()), Term.Name("ord"),
       Some(Type.Apply(Type.Name("Ord"), List(Type.Name("Int")))), None))), Type.Apply(Type.Name("Ord"),
        List(Type.Apply(Type.Name("List"), List(Type.Name("Int"))))), Term.Name("???")
         ) = stat
    }
  }

  test("given-alias-using-anonymous") {
   runTest("given (using ord: Ord[Int]) as Ord[List[Int]] = ???") { stat =>
      val Defn.GivenAlias(Nil, meta.Name.Anonymous(), Nil, List(List(Term.Param(List(Mod.Using()), Term.Name("ord"),
       Some(Type.Apply(Type.Name("Ord"), List(Type.Name("Int")))), None))), Type.Apply(Type.Name("Ord"),
        List(Type.Apply(Type.Name("List"), List(Type.Name("Int"))))), Term.Name("???")
         ) = stat
   }
  }

  test("given-alias-inline-subtype") {
    runTest("inline given intOrd as _ <: Ord[Int] = ???") { stat =>
      val Defn.GivenAlias(List(Mod.Inline()), Type.Name("intOrd"), Nil, Nil,
        Type.Placeholder(Type.Bounds(None, Some(Type.Apply(Type.Name("Ord"), List(Type.Name("Int")))))), Term.Name("???")
        ) = stat
    }
  }

  test("given-alias-subtype-noinline-error".ignore) {
    runTestError("given intOrd as _ <: Ord[Int] = ???", "is only allowed for given with inline modifier")
  }

  test("given-alias-combo") {
    runTest("inline given intOrd as _ <: Ord[Int] { val c: String } = ???") { stat =>
      val Defn.GivenAlias(List(Mod.Inline()), Type.Name("intOrd"), Nil, Nil,
        Type.Placeholder(Type.Bounds(None, Some(Type.Refine(Some(Type.Apply(Type.Name("Ord"), List(Type.Name("Int")))),
          List(Decl.Val(Nil, List(Pat.Var(Term.Name("c"))), Type.Name("String"))))))), Term.Name("???")
      ) = stat
    }
  }


  // ---------------------------------
  // USING
  // ---------------------------------

  test("using-named") {
    runTest("def f(a: Int)(using ord: UInt): Int = ???") { stat =>
      val Defn.Def(Nil, Term.Name("f"), Nil, List(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None)),
       List(Term.Param(List(Mod.Using()), Term.Name("ord"), Some(Type.Name("UInt")), None))), Some(Type.Name("Int")),
        Term.Name("???")
      ) = stat
    }
  }

  test("using-anonymous") {
    runTest("def f(a: Int)(using UInt): Int = ???") { stat =>
      val Defn.Def(Nil, Term.Name("f"), Nil, List(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None)),
       List(Term.Param(List(Mod.Using()), meta.Name.Anonymous(), Some(Type.Name("UInt")), None))), Some(Type.Name("Int")),
        Term.Name("???")
      ) = stat
    }
  }

  test("using-multiple-parens") {
    runTest("def f(a: Int)(using ui: UInt)(using us: UString): Boolean = ???") { stat =>
      val Defn.Def(Nil, Term.Name("f"), Nil, List(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None)),
       List(Term.Param(List(Mod.Using()), Term.Name("ui"), Some(Type.Name("UInt")), None)),
        List(Term.Param(List(Mod.Using()), Term.Name("us"), Some(Type.Name("UString")), None))),
         Some(Type.Name("Boolean")), Term.Name("???")
         ) = stat
    }
    runTest("def f(a: Int)(using UInt)(using UString): Boolean = ???") { stat =>
      val Defn.Def(Nil, Term.Name("f"), Nil, List(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None)),
       List(Term.Param(List(Mod.Using()), meta.Name.Anonymous(), Some(Type.Name("UInt")), None)),
        List(Term.Param(List(Mod.Using()), meta.Name.Anonymous(), Some(Type.Name("UString")), None))),
         Some(Type.Name("Boolean")), Term.Name("???")
         ) = stat
    }
  }

  test("using-many-single-paren") {
    runTest("def f(a: Int)(using ui: UInt, us: => UString): Boolean = ???") { stat =>
      val Defn.Def(Nil, Term.Name("f"), Nil, List(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None)),
        List(Term.Param(List(Mod.Using()), Term.Name("ui"), Some(Type.Name("UInt")), None), Term.Param(List(Mod.Using()),
         Term.Name("us"), Some(Type.ByName(Type.Name("UString"))), None))), Some(Type.Name("Boolean")), Term.Name("???")
      ) = stat
    }
    runTest("def f(a: Int)(using UInt, UString): Boolean = ???") { stat =>
      val  Defn.Def(Nil, Term.Name("f"), Nil, List(List(Term.Param(Nil, Term.Name("a"), Some(Type.Name("Int")), None)),
        List(Term.Param(List(Mod.Using()), Name(""), Some(Type.Name("UInt")), None), Term.Param(List(Mod.Using()), Name(""),
          Some(Type.Name("UString")), None))), Some(Type.Name("Boolean")), Term.Name("???")
          ) = stat
    }
  }

  test("using-complex") {
    runTest("def f(x: String)(using Int)(y: String)(using b: Int): Unit = ???") { stat =>
      val Defn.Def(Nil, Term.Name("f"), Nil, List(List(Term.Param(Nil, Term.Name("x"), Some(Type.Name("String")), None)),
        List(Term.Param(List(Mod.Using()), Name(""), Some(Type.Name("Int")), None)), List(Term.Param(Nil, Term.Name("y"),
          Some(Type.Name("String")), None)), List(Term.Param(List(Mod.Using()), Term.Name("b"), Some(Type.Name("Int")), None))),
            Some(Type.Name("Unit")), Term.Name("???")
      ) = stat
    }
  }

  test("using-mix-named-anonymous-error".ignore) {
    runTestError("def f(using a: Int, String): Unit = ???", "unable to mix named and anonymous using")
    runTestError("def f(using Int, b: String): Unit = ???", "unable to mix named and anonymous using")
  }

  test("using-multiple-using-single-parent-error".ignore) {
    runTestError("def f(using a: Int, using b: String): Unit = ???", "using only for first parameter")
  }

  test("using-added-middle-paren-error".ignore) {
    runTestError("def f(a: Int, using b: String): Unit = ???", "using only for first parameter")
  }

  test("using-named-with-by-name-parameter") {
    runTest("def f(using a: => Int): Unit = ???") { stat =>
      val Defn.Def(Nil, Term.Name("f"), Nil, List(List(Term.Param(List(Mod.Using()), Term.Name("a"),
        Some(Type.ByName(Type.Name("Int"))), None))), Some(Type.Name("Unit")), Term.Name("???")
      ) = stat
    }
  }

  test("using-anonymous-with-by-name-parameter-error".ignore) {
    runTestError("def f(using => String): Unit = ???", "anonymous using by-name invalid")
  }

  test("using-call-site") {
    //TODO: HOW TO TRANSLATE USING HERE???
    runTest("val a = f()(using a)(using 3, 'c')") { stat =>
      val Defn.Val(Nil, List(Pat.Var(Term.Name("a"))), None, Term.Apply(Term.Apply(Term.Apply(Term.Name("f"), Nil),
        List(Term.Select(Term.Name("using"), Term.Name("a")))), List(Term.Select(Term.Name("using"),
         Term.Name("b")), Term.Name("c")))
         ) = stat
    }
  }

  // ---------------------------------
  // IMPORT GIVEN
  // ---------------------------------

  // TODO: Add tests here

  private def runTest(code: String)(check: Stat => Unit) {
    implicit val dialect: Dialect = scala.meta.dialects.Dotty
    val stat = blockStat(code)
    try {
      check(stat)
    } catch {
      case e: MatchError =>
        println(s"Generated stat: \n ${stat.structure}")
        throw e
    }
  }

  private def runTestError(code: String, expected: String) {
    val error = intercept[ParseException](templStat(code))
    if (!error.getMessage().contains(error)) {
      println(s"Expected [${error.getMessage}] to contain [${expected}].")
    }
    assert(error.getMessage.contains(expected))
  }
}