package scala.meta.tests.parsers

import org.scalameta.logger
import scala.meta._
import scala.meta.parsers.Parse

import munit.Location

class StructureSuite extends ParseSuite {

  def checkStructure[T: Parse: Structure](code: String, expected: Tree)(implicit
      loc: Location
  ): Unit = test(logger.revealWhitespace(code)) {
    val obtained: String = code.parse[T].get.structure
    assertNoDiff(obtained, expected.structure)
  }

  checkStructure[Case]("case _ => _ => false", Case(patwildcard, None, tfunc(tparam("_"))(bool(false))))

  checkStructure[Case](
    "case _ => _ => {false}",
    Case(patwildcard, None, tfunc(tparam("_"))(blk(bool(false))))
  )

  checkStructure[Case](
    "case _ => _ => false; a",
    Case(patwildcard, None, tfunc(tparam("_"))(blk(bool(false), tname("a"))))
  )
}
