package scala.meta.tests.semanticdb

import scala.meta.interactive.InteractiveSemanticdb
import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.meta.internal.{semanticdb => s}

import scala.tools.nsc.reporters.StoreReporter

class DocumentBuildSuite extends SemanticdbSuite {

  test("two builds on one typed unit") {
    val code =
      """|object A {
         |  def f(xs: List[Int]): Unit = {
         |    val (a, b) = (1, "b")
         |    val pf: PartialFunction[Int, Int] = { case y if y > a => y }
         |    val r = new Runnable { def run(): Unit = () }
         |    for (x <- xs; z = x + a if z > 0) println(s"$z $b")
         |    @SuppressWarnings(Array("all")) val w = pf.lift(a)
         |    class C(val c: Int)
         |    println((r, w, new C(a).c))
         |  }
         |}
         |""".stripMargin
    val unit = typedUnit(code)
    val first = documentOf(unit)
    assertEquals(documentOf(unit), first)
  }

  private val unusedImport =
    """|import scala.collection.mutable
       |object B
       |""".stripMargin

  test("two builds on one typed unit with a warning") {
    val unit = typedUnit(unusedImport)
    val first = documentOf(unit)
    assertEquals((first.diagnostics.length, documentOf(unit).diagnostics.length), (1, 0))
  }

  test("two builds on one typed unit, a message between them") {
    val unit = typedUnit(unusedImport)
    val first = documentOf(unit)
    g.reporter.warning(unit.body.pos.focus, "by hand")
    assertEquals(
      (first.diagnostics.map(_.message), documentOf(unit).diagnostics.map(_.message)),
      (List("Unused import"), List("by hand")),
    )
  }

  test("two builds on one typed unit, a warning in another unit of the run") {
    val other =
      """|import scala.collection.immutable
         |object C
         |""".stripMargin
    val unit = typedUnit(unusedImport, other)
    val first = documentOf(unit)
    assertEquals(
      (first.diagnostics.map(_.range), documentOf(unit).diagnostics.map(_.range)),
      (List(Some(s.Range(0, 24, 0, 31))), Nil),
    )
  }

  /** a presentation compiler that holds a unit of `unusedImport`, as Metals holds a file */
  private class Presentation {
    val pc = InteractiveSemanticdb.newCompiler(List(warnUnusedImports))
    private val ops = new SemanticdbOps {
      val global: pc.type = pc
    }
    import ops._
    private val unit = InteractiveSemanticdb.addCompilationUnit(pc, unusedImport, "B.scala")

    /** the reporter's messages after a reset and a `typeCheck`, as at a Metals request */
    def typeCheck(): List[String] = {
      pc.reporter.reset()
      pc.typeCheck(unit)
      pc.reporter.asInstanceOf[StoreReporter].infos.toList.map(_.msg)
    }
    def diagnostics: List[String] = unit.toTextDocument.diagnostics.map(_.message).toList
  }

  private def withPresentation(f: Presentation => Unit): Unit = {
    val presentation = new Presentation
    try f(presentation)
    finally presentation.pc.askShutdown()
  }

  test("a presentation compiler: reset, typeCheck and build, twice on one unit")(withPresentation {
    p =>
      val first = (p.typeCheck(), p.diagnostics)
      val second = (p.typeCheck(), p.diagnostics)
      assertEquals((first, second), ((List("Unused import"), List("Unused import")), (Nil, Nil)))
  })

  test("a presentation compiler: typeCheck, then reset, typeCheck and build, on one unit")(
    withPresentation { p =>
      // a Metals request that types the unit and builds no document, such as a hover
      val typed = p.typeCheck()
      p.typeCheck()
      assertEquals((typed, p.diagnostics), (List("Unused import"), Nil))
    },
  )

}
