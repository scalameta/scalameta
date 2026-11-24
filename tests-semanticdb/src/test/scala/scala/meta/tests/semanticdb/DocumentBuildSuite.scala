package scala.meta.tests.semanticdb

import scala.meta.interactive.InteractiveSemanticdb
import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.meta.internal.{semanticdb => s}

import java.lang.ref.WeakReference

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
    assertEquals((first.diagnostics.length, documentOf(unit).diagnostics.length), (1, 1))
  }

  test("two builds on one typed unit, a message between them") {
    val unit = typedUnit(unusedImport)
    val first = documentOf(unit)
    g.reporter.warning(unit.body.pos.focus, "by hand")
    assertEquals(
      (first.diagnostics.map(_.message), documentOf(unit).diagnostics.map(_.message)),
      (List("Unused import"), List("by hand", "Unused import")),
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
      (List(Some(s.Range(0, 24, 0, 31))), List(Some(s.Range(0, 24, 0, 31)))),
    )
  }

  private val localVal =
    """|object L {
       |  def f(): Int = {
       |    val x = 1
       |    x
       |  }
       |}
       |""".stripMargin

  private val otherFile = localVal.replace("L", "G")

  /** a presentation compiler that holds a unit of `code`, as Metals holds a file */
  private class Presentation(code: String) {

    val pc = InteractiveSemanticdb.newCompiler(List(warnUnusedImports))
    val ops = new SemanticdbOps {
      val global: pc.type = pc
    }
    import ops._
    private var unit = InteractiveSemanticdb.addCompilationUnit(pc, code, "B.scala")

    /** replaces the unit of the file with one of `code`, as Metals does at an edit */
    def edit(code: String): Unit =
      unit = InteractiveSemanticdb.addCompilationUnit(pc, code, "B.scala")

    /** drops the unit of the file, as Metals evicts a file, and holds a unit of another file */
    def evict(): Unit = {
      pc.unitOfFile.remove(unit.source.file)
      unit = InteractiveSemanticdb.addCompilationUnit(pc, otherFile, "G.scala")
    }
    def source: AnyRef = unit.source
    def input: AnyRef = unit.source.toInput

    /** whether two sources of one file are equal, as scalac 2.12 has them */
    val isSourceEqualByPath: Boolean = pc.newSourceFile("", "B.scala") ==
      pc.newSourceFile("", "B.scala")

    /**
     * reads a tree of a file that has no unit, as Metals does at a symbol of a file that it
     * dropped: the compiler starts a new run
     */
    def typedTreeOfNewFile(): Unit = pc
      .typedTreeAt(pc.newSourceFile("object C\n", "C.scala").position(0))

    /** a hover on a local of another file, then a document of that file, as at Metals requests */
    def requestsOnAnotherFile(): Unit = {
      val other = InteractiveSemanticdb.addCompilationUnit(pc, otherFile, "G.scala")
      pc.typeCheck(other)
      other.body.foreach {
        case t: pc.ValDef if t.symbol.isLocalToBlock => t.symbol.toSemantic
        case _ =>
      }
      other.toTextDocument
    }

    /** the reporter's messages after a reset and a `typeCheck`, as at a Metals request */
    def typeCheck(): List[String] = {
      pc.reporter.reset()
      pc.typeCheck(unit)
      pc.reporter.asInstanceOf[StoreReporter].infos.toList.map(_.msg)
    }
    def diagnostics: List[String] = unit.toTextDocument.diagnostics.map(_.message).toList
    def document: s.TextDocument = unit.toTextDocument

    /** removes the unit's source from `gSourceFileInputCache`, as Metals does before a document */
    def removeInput(): Unit = gSourceFileInputCache.remove(unit.source)

    /** clears every cache of `ops`, as the plugin does after a run */
    def clearCaches(): Unit = {
      ops.clearSymbolCaches()
      ops.gSourceFileInputCache.clear()
    }

    /** the symbols of the unit's local values, outside a document, as Metals asks at a hover */
    def localSymbols: List[String] = unit.body
      .collect { case t: pc.ValDef if t.symbol.isLocalToBlock => t.symbol.toSemantic }

    /** the symbol of the unit's local value `name`, outside a document */
    def localSymbol(name: String): List[String] = unit.body.collect {
      case t: pc.ValDef if t.symbol.isLocalToBlock && t.name.toString == name => t.symbol.toSemantic
    }

    /** the symbols of the unit's binders and local values, outside a document, in tree order */
    def binderSymbols: String = unit.body.collect {
      case t @ (_: pc.Bind | _: pc.ValDef) if t.symbol.isLocalToBlock =>
        s"${t.symbol.name}=${t.symbol.toSemantic}"
    }.mkString(" ")
    def local: pc.Symbol = unit.body
      .collect { case t: pc.ValDef if t.symbol.isLocalToBlock => t.symbol }.head
    def method: pc.Symbol = unit.body
      .collect { case t: pc.DefDef if t.name.toString == "f" => t.symbol }.head

    /** the symbol of method `f`, outside a document, as Metals asks at a hover */
    def methodSymbol: String = method.toSemantic

  }

  /** whether a full collection clears `ref` */
  private def isCollected(ref: WeakReference[_ <: AnyRef]): Boolean = (1 to 3).exists { _ =>
    System.gc()
    Thread.sleep(10)
    ref.get eq null
  }

  private def withPresentation(f: Presentation => Unit, code: String = unusedImport): Unit = {
    val presentation = new Presentation(code)
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

  test("a presentation compiler: a local's symbol, typeCheck twice on one unit")(withPresentation(
    { p =>
      p.typeCheck()
      val first = p.localSymbols
      p.typeCheck()
      assertEquals((first, p.localSymbols), (List("local0"), List("local0")))
    },
    localVal,
  ))

  test("a presentation compiler: an edit after a document build")(withPresentation(
    { p =>
      p.typeCheck()
      p.diagnostics
      val old = new WeakReference(p.source)
      p.edit(localVal.replace("1", "2"))
      p.typeCheck()
      p.diagnostics
      val beforeClear = isCollected(old)
      p.clearCaches()
      assertEquals((beforeClear, isCollected(old)), (false, false))
    },
    localVal,
  ))

  test("control: an edit without SemanticdbOps")(withPresentation(
    { p =>
      p.typeCheck()
      val old = new WeakReference(p.source)
      p.edit(localVal.replace("1", "2"))
      p.typeCheck()
      assertEquals(isCollected(old), false)
    },
    localVal,
  ))

  test("a presentation compiler: a method's symbol, typeCheck twice on one unit")(withPresentation(
    { p =>
      p.typeCheck()
      val first = (p.method, p.methodSymbol)
      p.typeCheck()
      assertEquals(
        (first._1 eq p.method, first._2, p.methodSymbol),
        (true, "_empty_/L.f().", "_empty_/L.f()."),
      )
    },
    localVal,
  ))

  test("a presentation compiler: an edit after a document build, then a tree of a new file")(
    withPresentation(
      { p =>
        p.typeCheck()
        p.diagnostics
        val old = new WeakReference(p.source)
        p.edit(localVal.replace("1", "2"))
        p.typeCheck()
        p.diagnostics
        p.typedTreeOfNewFile()
        val beforeClear = isCollected(old)
        p.clearCaches()
        // scalac 2.13 keeps each unit that it typed in `analyzer.rootImportsCached`
        val expected = if (isScala213) (false, false) else (false, true)
        assertEquals((beforeClear, isCollected(old)), expected)
      },
      localVal,
    ),
  )

  test("a presentation compiler: an edit after a local's symbol, then a tree of a new file")(
    withPresentation(
      { p =>
        p.typeCheck()
        p.localSymbols
        val old = new WeakReference(p.source)
        p.edit(localVal.replace("1", "2"))
        p.typeCheck()
        p.localSymbols
        p.typedTreeOfNewFile()
        val beforeClear = isCollected(old)
        p.ops.clearSymbolCaches()
        val expected = if (isScala213) (false, false) else (false, true)
        assertEquals((beforeClear, isCollected(old)), expected)
      },
      localVal,
    ),
  )

  test("a presentation compiler: an edit after a method's symbol, then a tree of a new file")(
    withPresentation(
      { p =>
        p.typeCheck()
        p.methodSymbol
        val old = new WeakReference(p.method)
        p.edit(localVal.replace("1", "2"))
        p.typeCheck()
        p.methodSymbol
        p.typedTreeOfNewFile()
        val beforeClear = isCollected(old)
        p.ops.clearSymbolCaches()
        val expected = if (isScala213) (false, false) else (false, true)
        assertEquals((beforeClear, isCollected(old)), expected)
      },
      localVal,
    ),
  )

  test("control: an edit, then a tree of a new file, without SemanticdbOps")(withPresentation(
    { p =>
      p.typeCheck()
      val old = new WeakReference(p.source)
      p.edit(localVal.replace("1", "2"))
      p.typeCheck()
      p.typedTreeOfNewFile()
      assertEquals(isCollected(old), !isScala213)
    },
    localVal,
  ))

  test("a presentation compiler: a local's symbol, an edit twice")(withPresentation(
    { p =>
      p.typeCheck()
      val first = p.localSymbols
      p.edit(localVal.replace("1", "2"))
      p.typeCheck()
      val second = p.localSymbols
      p.edit(localVal.replace("1", "3"))
      p.typeCheck()
      val expected = if (p.isSourceEqualByPath) "local0 local1 local2" else "local0 local0 local0"
      assertEquals((first ++ second ++ p.localSymbols).mkString(" "), expected)
    },
    localVal,
  ))

  private def occurrenceLines(d: s.TextDocument): String = d.occurrences
    .map(_.range.fold(-1)(_.startLine)).mkString(" ")

  test("a presentation compiler: a document after an edit")(withPresentation(
    { p =>
      p.typeCheck()
      val first = p.document
      p.edit(localVal.replace("object L", "\nobject L"))
      p.typeCheck()
      val second = p.document
      val expected =
        if (p.isSourceEqualByPath) (true, "0 1 1 2 3", "") else (false, "0 1 1 2 3", "1 2 2 3 4")
      assertEquals(
        (first.md5 == second.md5, occurrenceLines(first), occurrenceLines(second)),
        expected,
      )
    },
    localVal,
  ))

  test("a presentation compiler: a document after an edit, its input removed as Metals does")(
    withPresentation(
      { p =>
        p.typeCheck()
        val first = p.document
        p.edit(localVal.replace("object L", "\nobject L"))
        p.typeCheck()
        p.removeInput()
        val second = p.document
        assertEquals(
          (first.md5 == second.md5, occurrenceLines(first), occurrenceLines(second)),
          (false, "0 1 1 2 3", "1 2 2 3 4"),
        )
      },
      localVal,
    ),
  )

  test("a presentation compiler: an edit after a document build, a tree of a new file, a build")(
    withPresentation(
      { p =>
        p.typeCheck()
        p.diagnostics
        val old = new WeakReference(p.source)
        p.edit(localVal.replace("1", "2"))
        p.typeCheck()
        p.diagnostics
        p.typedTreeOfNewFile()
        p.diagnostics
        assertEquals(isCollected(old), false)
      },
      localVal,
    ),
  )

  test("a presentation compiler: an edit after a local's symbol, a tree of a new file, a method")(
    withPresentation(
      { p =>
        p.typeCheck()
        p.localSymbols
        val old = new WeakReference(p.source)
        p.edit(localVal.replace("1", "2"))
        p.typeCheck()
        p.localSymbols
        p.typedTreeOfNewFile()
        p.methodSymbol
        assertEquals(isCollected(old), false)
      },
      localVal,
    ),
  )

  test("a presentation compiler: an edit after a method's symbol, a tree of a new file, a local")(
    withPresentation(
      { p =>
        p.typeCheck()
        p.methodSymbol
        val old = new WeakReference(p.method)
        p.edit(localVal.replace("1", "2"))
        p.typeCheck()
        p.methodSymbol
        p.typedTreeOfNewFile()
        p.localSymbols
        assertEquals(isCollected(old), false)
      },
      localVal,
    ),
  )

  private val forBinders =
    """|object L {
       |  val xs = List(1)
       |  def f(): List[Int] = {
       |    val Some(a) = xs.headOption
       |    for (x <- xs; y = x + a) yield y
       |  }
       |}
       |""".stripMargin

  test("a presentation compiler: binders' symbols, a tree of a new file, requests on another file")(
    withPresentation(
      { p =>
        p.typeCheck()
        val first = p.binderSymbols
        p.typedTreeOfNewFile()
        p.requestsOnAnotherFile()
        p.typeCheck()
        assertEquals(
          (first, p.binderSymbols),
          (
            "a=local0 a=local0 x=local1 y=local2 x$1=local3 x=local1 y=local2",
            "a=local0 a=local4 x=local1 y=local2 x$1=local3 x=local1 y=local2",
          ),
        )
      },
      forBinders,
    ),
  )

  test("a presentation compiler: a local's symbol, a build, an eviction, a tree of a new file")(
    withPresentation(
      { p =>
        p.typeCheck()
        p.localSymbols
        p.diagnostics
        val old = new WeakReference(p.local)
        p.evict()
        p.typedTreeOfNewFile()
        p.typeCheck()
        p.methodSymbol
        val beforeClear = isCollected(old)
        p.clearCaches()
        val expected = if (isScala213) (false, false) else (false, true)
        assertEquals((beforeClear, isCollected(old)), expected)
      },
      localVal,
    ),
  )

  test("control: an eviction, then a tree of a new file, without SemanticdbOps")(withPresentation(
    { p =>
      p.typeCheck()
      val old = new WeakReference(p.local)
      p.evict()
      p.typedTreeOfNewFile()
      p.typeCheck()
      assertEquals(isCollected(old), !isScala213)
    },
    localVal,
  ))

  test("a presentation compiler: a local's symbol, a document build, the symbol again")(
    withPresentation(
      { p =>
        p.typeCheck()
        val hover = p.localSymbol("y")
        p.diagnostics
        assertEquals((hover, p.localSymbol("y")), (List("local0"), List("local1")))
      },
      """|object L {
         |  def f(): Int = {
         |    val x = 1
         |    val y = 2
         |    x + y
         |  }
         |}
         |""".stripMargin,
    ),
  )

  test("a presentation compiler: an edit after a document build, the old input")(withPresentation(
    { p =>
      p.typeCheck()
      p.diagnostics
      val old = new WeakReference(p.input)
      p.edit(localVal.replace("1", "2"))
      p.typeCheck()
      p.diagnostics
      assertEquals(isCollected(old), false)
    },
    localVal,
  ))

}
