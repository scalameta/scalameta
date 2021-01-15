package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class ExportSuite extends BaseDottySuite {

  implicit val parseStat: String => Stat = code => templStat(code)(dialects.Dotty)

  test("export-single") {
    runTestAssert[Stat]("export A.b.c")(
      Export(
        List(Importer(Term.Select(Term.Name("A"), Term.Name("b")), List(Importee.Name(Name("c")))))
      )
    )
  }

  test("export-given") {
    runTestAssert[Stat]("export A.{ given Int }")(
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
    runTestAssert[Stat]("export A.{ b, c, d, _ }")(
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
    runTestAssert[Stat]("export A.{ b => x, c => _ }")(
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
}
