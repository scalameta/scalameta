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
   */

    //TODO: CHECK ALL STATEMENTS COMPILE IN DOTTY here in tests!

    //TODO: HANDLE INLINE

   /* ------------ GIVEN ----------- */
  test("given-named") {
    runTest("given intOrd as Ord[Int] { def f(): Int = 1 }") { stat =>
      val Defn.Given(Nil, meta.Name("intOrd"), Nil, Nil, Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
        Term.Block(List(Defn.Def(Nil, Term.Name("f"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(1))))
      ) = stat
    }
  }

  test("given-override-def".ignore) {
    //TODO: NOT WORKING YET
    // Standard given but compared to standard block method can have Mod 'override'
    implicit val dialect: Dialect = scala.meta.dialects.Dotty
    val code = "given intOrd as Ord[Int] { override def f(): Int = 1 }"

    val Defn.Given(Nil, meta.Name("intOrd"), Nil, Nil, Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
      Term.Block(List(Term.Block(List(Defn.Def(List(Mod.Override()), Term.Name("f"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(1))))))
    ) = templStat(code)
  }

  test("given-anonymous") {
    runTest("given Ord[Int] { def f(): Int = 1 }") { stat =>
    val Defn.Given(Nil, meta.Name.Anonymous(), Nil, Nil, Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
      Term.Block(List(Defn.Def(Nil, Term.Name("f"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(1))))
    ) = stat
    }
  }

  test("given-no-block".ignore) {
    implicit val dialect: Dialect = scala.meta.dialects.Dotty
    val code = "given intOrd as Ord[Int]"

    val Defn.Given(Nil, meta.Name.Anonymous(), Nil, Nil, Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
      Term.Block(List(Term.Block(List())))
    ) = templStat(code)
  }

  test("given-anonymous-no-block".ignore) {
    implicit val dialect: Dialect = scala.meta.dialects.Dotty
    val code = "given Ord[Int]"

    val Defn.Given(Nil, meta.Name.Anonymous(), Nil, Nil, Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
      Term.Block(List(Term.Block(List())))
    ) = templStat(code)
  }

  test("given-generic-named") {
    runTest("given listOrd[T] as Ord[List[T]] { def f(): Int = 1 }") { stat =>
    val Defn.Given(Nil, meta.Name("listOrd"), List(Type.Param(Nil, Type.Name("T"), Nil, _, Nil, Nil)), Nil,
      Type.Apply(Type.Name("Ord"), List(Type.Apply(Type.Name("List"), List(Type.Name("T"))))),
        Term.Block(List(Defn.Def(Nil, Term.Name("f"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(1))))
        ) = stat
    }
  }

  test("given-generic-anonymous") {
    runTest("given Ord[List[T]] { def f(): Int = 1 }") { stat =>
      val Defn.Given(Nil, meta.Name.Anonymous(), Nil, Nil, Type.Apply(Type.Name("Ord"),
       List(Type.Apply(Type.Name("List"), List(Type.Name("T"))))),
         Term.Block(List(Defn.Def(Nil, Term.Name("f"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(1))))
      ) = stat
    }
  }

  test("given-depend-given-named") {
    runTest("given setOrd[T](using ord: Ord[T]) as Ord[Set[T]] { def f(): Int = 1 }") { stat =>
      val Defn.Given(Nil, meta.Name("setOrd"), List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        List(List(Term.Param(List(Mod.Using()), Term.Name("ord"), Some(Type.Apply(Type.Name("Ord"), List(Type.Name("T")))), None))),
        Type.Apply(Type.Name("Ord"), List(Type.Apply(Type.Name("Set"), List(Type.Name("T"))))),
        Term.Block(List(Defn.Def(Nil, Term.Name("f"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(1))))
      ) = stat
    }
  }

  test("given-depend-given-anonymous") {
    runTest("given [T](using ord: Ord[T]) as Ord[Set[T]] { def f(): Int = 1 }") { stat =>
      val Defn.Given(Nil, meta.Name.Anonymous(), List(Type.Param(Nil, Type.Name("T"), Nil, Type.Bounds(None, None), Nil, Nil)),
        List(List(Term.Param(List(Mod.Using()), Term.Name("ord"), Some(Type.Apply(Type.Name("Ord"), List(Type.Name("T")))), None))),
        Type.Apply(Type.Name("Ord"), List(Type.Apply(Type.Name("Set"), List(Type.Name("T"))))),
        Term.Block(List(Defn.Def(Nil, Term.Name("f"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(1))))
      ) = stat
    }
  }

  test("given-alias-named") {
    runTest("given global as Option[Int] = Some(3)") { stat =>
      val Defn.Given(Nil, meta.Name("global"), Nil, Nil, Type.Apply(Type.Name("Option"), List(Type.Name("Int"))),
        Term.Apply(Term.Name("Some"), List(Lit.Int(3)))
      ) = stat
    }
  }

  test("given-alias-anonymous") {
    runTest("given Option[Int] = Some(3)") { stat =>
      val Defn.Given(Nil, meta.Name.Anonymous(), Nil, Nil, Type.Apply(Type.Name("Option"), List(Type.Name("Int"))),
        Term.Apply(Term.Name("Some"), List(Lit.Int(3)))
         ) = stat
    }

  }

  test("given-alias-block") {
    runTest("given global as Option[Int] = { def fx(): Int = 3; Some(3) }") { stat =>
      val Defn.Given(Nil, Type.Name("global"), Nil, Nil, Type.Apply(Type.Name("Option"), List(Type.Name("Int"))),
       Term.Block(List(Defn.Def(Nil, Term.Name("fx"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(3)),
        Term.Apply(Term.Name("Some"), List(Lit.Int(3)))))
         ) = stat

    }
  }

  //TODO: equal with block definition reject override etc. (no rejection tests so far)
  //given global as Option[Int] = { override def fx(): Int = 3; Some(3) }

  test("given-alias-using-named") {
   runTest("given ordInt(using ord: Ord[Int]) as Ord[List[Int]] = ???") { stat =>
      val Defn.Given(Nil, meta.Name("ordInt"), Nil, List(List(Term.Param(List(Mod.Using()), Term.Name("ord"),
       Some(Type.Apply(Type.Name("Ord"), List(Type.Name("Int")))), None))), Type.Apply(Type.Name("Ord"),
        List(Type.Apply(Type.Name("List"), List(Type.Name("Int"))))), Term.Name("???")
         ) = stat
  }

  }

  test("given-alias-using-anonymous") {
   runTest("given (using ord: Ord[Int]) as Ord[List[Int]] = ???") { stat =>
      val Defn.Given(Nil, meta.Name.Anonymous(), Nil, List(List(Term.Param(List(Mod.Using()), Term.Name("ord"),
       Some(Type.Apply(Type.Name("Ord"), List(Type.Name("Int")))), None))), Type.Apply(Type.Name("Ord"),
        List(Type.Apply(Type.Name("List"), List(Type.Name("Int"))))), Term.Name("???")
         ) = stat
   }
  }

  test("given-inline") {
    runTest("inline given intOrd as Ord[Int] { def f(): Int = 1 }") { stat =>
      val Defn.Given(List(Mod.Inline()), meta.Name("intOrd"), Nil, Nil, Type.Apply(Type.Name("Ord"), List(Type.Name("Int"))),
        Term.Block(List(Defn.Def(Nil, Term.Name("f"), Nil, List(List()), Some(Type.Name("Int")), Lit.Int(1))))
      ) = stat
    }
  }

  test("given-inline-subtype") {
    runTest("inline given intOrd as _ <: Ord[Int] = ???") { stat =>
      val Defn.Given(List(Mod.Inline()), Type.Name("intOrd"), Nil, Nil,
        Type.Placeholder(Type.Bounds(None, Some(Type.Apply(Type.Name("Ord"), List(Type.Name("Int")))))), Term.Name("???")
        ) = stat
    }
  }



  //TODO: Add test to reject when _ <: without inline
  //TODO: Add test to reject _ <: when used without '='

   /* ------------ USING ----------- */

   /** Using
    * 
    * trait Ord[T] { def cmp(x: T, y: T): Int }
    * 
    * //Basic use
    * def f[T](a: T)(using ord: Ord[T]): T = ???
    * 
    * //Anonymous context
    * def f[T](a: T)(using Ord[T]): T = ???
    * 
    * //Multiple using clauses
    * def f(a: Int)(using Ord[Int])(using Ord[String]): Boolean = ???
    * 
    * //Multiple usings in single clause
    * def f(a: Int)(using ui: Ord[Int], us: Ord[String]): Boolean = ???
    * def f(a: Int)(using Ord[Int], Ord[String]): Boolean = ???
    * 
    * //Summon global method
    * def summon[T](using x: T): x.type = x  // it can be used anywhere to summon implicit instance (implicitly?)
    *
    * 
    */

   
  private def runTest(code: String)(check: Stat => Unit) {
    implicit val dialect: Dialect = scala.meta.dialects.Dotty
    val stat = templStat(code)
    try {
      check(stat)
    } catch {
      case e: MatchError =>
        println(s"Generated stat: \n ${stat.structure}")
        throw e
    }
  }
}