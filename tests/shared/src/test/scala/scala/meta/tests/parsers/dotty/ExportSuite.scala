package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

class ExportSuite extends BaseDottySuite {

  implicit val parseStat: String => Stat = code => templStat(code)(dialects.Dotty)

  test("export-single") {
    runTestAssert[Stat]("export A.b.c")(
      Export(
        false,
        List(Importer(Term.Select(Term.Name("A"), Term.Name("b")), List(Importee.Name(Name("c")))))
      )
    )
  }

  test("export-given") {
    runTestAssert[Stat]("export given A.b.c")(
      Export(
        true,
        List(Importer(Term.Select(Term.Name("A"), Term.Name("b")), List(Importee.Name(Name("c")))))
      )
    )
  }

  test("export-multiple") {
    runTestAssert[Stat]("export A.{ b, c, d, _ }")(
      Export(
        false,
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
        false,
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
