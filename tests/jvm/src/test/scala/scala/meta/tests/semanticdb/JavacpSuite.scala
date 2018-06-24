package scala.meta.tests.semanticdb

import org.scalatest.FunSuite
import scala.meta.internal.semanticdb.ClassSignature
import scala.meta.internal.semanticdb.Scala._

class JavacpSuite extends FunSuite {
  private val infos = MetacMetacpExpectDiffExpect.metacpSymbols

  def checkOrder(
      name: String,
      symbol: String,
      filter: String => Boolean,
      expected: List[String]): Unit =
    test(name) {
      val info = infos(symbol)
      val ClassSignature(_, _, _, Some(declarations)) = info.signature
      val obtained = declarations.symlinks.filter(filter)
      assert(obtained == expected, info.toProtoString)
    }

  checkOrder(
    "methods",
    "com.javacp.MetacJava#",
    _.desc.name == "overload",
    List(
      "com.javacp.MetacJava#overload().",
      "com.javacp.MetacJava#overload(+2).",
      "com.javacp.MetacJava#overload(+1)."
    )
  )

  checkOrder(
    "fields",
    "com.javacp.Test#", { s =>
      s.desc.name == "Int" ||
      s.desc.name == "Long" ||
      s.desc.name == "Float"
    },
    List(
      "com.javacp.Test#Int.",
      "com.javacp.Test#Long.",
      "com.javacp.Test#Float."
    )
  )

}
