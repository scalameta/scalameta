package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class ExportImportSuite extends BaseDottySuite {

  test("export-single") {
    runTestAssert[Stat]("export A.b.c")(
      Export(
        List(Importer(Term.Select(Term.Name("A"), Term.Name("b")), List(Importee.Name(Name("c")))))
      )
    )
  }

  test("export-single-toplevel") {
    runTestAssert[Source](
      """|package a
         |export A.b.c
         |""".stripMargin
    )(
      Source(
        List(
          Pkg(
            Term.Name("a"),
            List(
              Export(
                List(
                  Importer(
                    Term.Select(Term.Name("A"), Term.Name("b")),
                    List(Importee.Name(Name("c")))
                  )
                )
              )
            )
          )
        )
      )
    )
  }

  test("export-given") {
    runTestAssert[Stat]("export A.{ given Int }")(
      Export(List(Importer(Term.Name("A"), List(Importee.Given(Type.Name("Int"))))))
    )
  }

  test("export-given-no-brace") {
    runTestAssert[Stat](
      "export A.given Int",
      assertLayout = Some("export A.{ given Int }")
    )(
      Export(List(Importer(Term.Name("A"), List(Importee.Given(Type.Name("Int"))))))
    )
  }

  test("export-given-all") {
    runTestAssert[Stat]("export A.given")(
      Export(List(Importer(Term.Name("A"), List(Importee.GivenAll()))))
    )
  }

  test("export-given-all-mixed") {
    runTestAssert[Stat]("export A.{ given, a }")(
      Export(List(Importer(Term.Name("A"), List(Importee.GivenAll(), Importee.Name(Name("a"))))))
    )
  }

  test("export-multiple") {
    runTestAssert[Stat]("export A.{ b, c, d, * }")(
      Export(
        List(
          Importer(
            Term.Name("A"),
            List(
              Importee.Name(Name("b")),
              Importee.Name(Name("c")),
              Importee.Name(Name("d")),
              Importee.Wildcard()
            )
          )
        )
      )
    )
  }

  test("export-multiple-rename") {
    runTestAssert[Stat]("export A.{ b as x, c as _ }")(
      Export(
        List(
          Importer(
            Term.Name("A"),
            List(Importee.Rename(Name("b"), Name("x")), Importee.Unimport(Name("c")))
          )
        )
      )
    )
  }

  test("rename-as") {
    runTestAssert[Stat]("import a.b.C as D")(
      Import(
        List(
          Importer(
            Term.Select(Term.Name("a"), Term.Name("b")),
            List(Importee.Rename(Name("C"), Name("D")))
          )
        )
      )
    )
  }

  test("rename-as-simple") {
    runTestAssert[Stat]("import c as d")(
      Import(List(Importer(Term.Anonymous(), List(Importee.Rename(Name("c"), Name("d"))))))
    )
  }

  test("rename-multi") {
    runTestAssert[Stat]("import A.{ min as minimum, `*` as multiply }")(
      Import(
        List(
          Importer(
            Term.Name("A"),
            List(
              Importee.Rename(Name("min"), Name("minimum")),
              Importee.Rename(Name("*"), Name("multiply"))
            )
          )
        )
      )
    )
  }
  test("rename-multi-unimport") {
    runTestAssert[Stat]("import Predef.{ augmentString as _, * }")(
      Import(
        List(
          Importer(
            Term.Name("Predef"),
            List(Importee.Unimport(Name("augmentString")), Importee.Wildcard())
          )
        )
      )
    )
  }

  test("unimport-as") {
    runTestAssert[Stat]("import a.b.C as _")(
      Import(
        List(
          Importer(Term.Select(Term.Name("a"), Term.Name("b")), List(Importee.Unimport(Name("C"))))
        )
      )
    )
  }

  test("import-wildcard") {
    runTestAssert[Stat]("import A.b.*")(
      Import(
        List(Importer(Term.Select(Term.Name("A"), Term.Name("b")), List(Importee.Wildcard())))
      )
    )
  }

  test("import-wildcard-backquoted") {
    runTestAssert[Stat]("import A.b.`*`")(
      Import(
        List(Importer(Term.Select(Term.Name("A"), Term.Name("b")), List(Importee.Name(Name("*")))))
      )
    )
  }
}
