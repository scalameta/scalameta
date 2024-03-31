package scala.meta.tests.parsers

import munit.Location
import org.scalameta.logger

import scala.meta._
import scala.meta.parsers.Parse

class StructureSuite extends ParseSuite {

  def checkStructure[T: Parse: Structure](code: String, expected: Tree)(implicit
      loc: Location
  ): Unit = test(logger.revealWhitespace(code)) {
    val obtained: String = code.parse[T].get.structure
    assertNoDiff(obtained, expected.structure)
  }

  checkStructure[Case](
    "case _ => _ => false",
    Case(Pat.Wildcard(), None, Term.Function(List(tparam("_")), bool(false)))
  )

  checkStructure[Case](
    "case _ => _ => {false}",
    Case(Pat.Wildcard(), None, Term.Function(List(tparam("_")), Term.Block(List(bool(false)))))
  )

  checkStructure[Case](
    "case _ => _ => false; a",
    Case(
      Pat.Wildcard(),
      None,
      Term.Function(List(tparam("_")), Term.Block(List(bool(false), tname("a"))))
    )
  )
}
