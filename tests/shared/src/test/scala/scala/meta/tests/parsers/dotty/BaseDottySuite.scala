package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

trait BaseDottySuite extends ParseSuite {

  protected implicit val dialect: Dialect = dialects.Scala3

}
