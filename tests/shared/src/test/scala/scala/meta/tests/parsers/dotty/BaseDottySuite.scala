package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.tests.parsers._

trait BaseDottySuite extends ParseSuite {

  protected implicit val dialect: Dialect = dialects.Scala3

}
