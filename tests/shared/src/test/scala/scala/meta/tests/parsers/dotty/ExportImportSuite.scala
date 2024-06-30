package scala.meta.tests.parsers.dotty

import scala.meta._

class ExportImportSuite extends BaseDottySuite {

  test("export-single") {
    runTestAssert[Stat]("export A.b.c")(Export(
      List(Importer(Term.Select(tname("A"), tname("b")), List(Importee.Name(Name("c")))))
    ))
  }

  test("export-single-toplevel") {
    runTestAssert[Source](
      """|package a
         |export A.b.c
         |""".stripMargin
    )(Source(List(Pkg(
      tname("a"),
      List(Export(List(Importer(Term.Select(tname("A"), tname("b")), List(Importee.Name(Name("c")))))))
    ))))
  }

  test("export-given") {
    runTestAssert[Stat]("export A.{ given Int }", assertLayout = Some("export A.given Int"))(Export(
      List(Importer(tname("A"), List(Importee.Given(pname("Int")))))
    ))
  }

  test("export-given-no-brace") {
    runTestAssert[Stat]("export A.given Int", assertLayout = Some("export A.given Int"))(Export(
      List(Importer(tname("A"), List(Importee.Given(pname("Int")))))
    ))
  }

  test("export-given-all") {
    runTestAssert[Stat]("export A.given")(Export(
      List(Importer(tname("A"), List(Importee.GivenAll())))
    ))
  }

  test("export-given-all-mixed") {
    runTestAssert[Stat]("export A.{ given, a }")(Export(
      List(Importer(tname("A"), List(Importee.GivenAll(), Importee.Name(Name("a")))))
    ))
  }

  test("export-multiple") {
    runTestAssert[Stat]("export A.{ b, c, d, * }")(Export(List(Importer(
      tname("A"),
      List(
        Importee.Name(Name("b")),
        Importee.Name(Name("c")),
        Importee.Name(Name("d")),
        Importee.Wildcard()
      )
    ))))
  }

  test("export-multiple-rename") {
    runTestAssert[Stat]("export A.{ b as x, c as _ }")(Export(List(
      Importer(tname("A"), List(Importee.Rename(Name("b"), Name("x")), Importee.Unimport(Name("c"))))
    )))
  }

  test("rename-as") {
    runTestAssert[Stat]("import a.b.C as D")(Import(List(
      Importer(Term.Select(tname("a"), tname("b")), List(Importee.Rename(Name("C"), Name("D"))))
    )))
  }

  test("rename-as-simple") {
    runTestAssert[Stat]("import c as d")(Import(
      List(Importer(Term.Anonymous(), List(Importee.Rename(Name("c"), Name("d")))))
    ))
  }

  test("rename-multi") {
    runTestAssert[Stat]("import A.{ min as minimum, `*` as multiply }")(Import(List(Importer(
      tname("A"),
      List(Importee.Rename(Name("min"), Name("minimum")), Importee.Rename(Name("*"), Name("multiply")))
    ))))
  }
  test("rename-multi-unimport") {
    runTestAssert[Stat]("import Predef.{ augmentString as _, * }")(Import(List(
      Importer(tname("Predef"), List(Importee.Unimport(Name("augmentString")), Importee.Wildcard()))
    )))
  }

  test("unimport-as") {
    runTestAssert[Stat]("import a.b.C as _")(Import(
      List(Importer(Term.Select(tname("a"), tname("b")), List(Importee.Unimport(Name("C")))))
    ))
  }

  test("import-wildcard") {
    runTestAssert[Stat]("import A.b.*")(Import(
      List(Importer(Term.Select(tname("A"), tname("b")), List(Importee.Wildcard())))
    ))
  }

  test("import-wildcard-backquoted") {
    runTestAssert[Stat]("import A.b.`*`")(Import(
      List(Importer(Term.Select(tname("A"), tname("b")), List(Importee.Name(Name("*")))))
    ))
  }

  test("#3754 export Type.this.field") {
    val code = """|case class A():
                  |  export A.this.value
                  |""".stripMargin
    val layout = "case class A() { export A.this.value }"
    val tree = Defn.Class(
      List(Mod.Case()),
      pname("A"),
      Nil,
      ctorp(Nil),
      tpl(Export(List(Importer(Term.This(Name("A")), List(Importee.Name(Name("value")))))))
    )
    runTestAssert[Stat](code, layout)(tree)
  }

}
