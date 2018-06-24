package scala.meta.tests.semanticdb

import org.scalatest.FunSuite
import scala.meta.internal.semanticdb.ClassSignature
import scala.meta.internal.semanticdb.Scala._

class JavacpSuite extends FunSuite {
  test("ClassSignature.declarations order") {
    val info = MetacMetacpExpectDiffExpect.metacpSymbols("com.javacp.MetacJava#")
    val ClassSignature(_, _, _, Some(declarations)) = info.signature
    val obtained = declarations.symlinks.filter(_.desc.name == "overload")
    val expected = List(
      "com.javacp.MetacJava#overload().",
      "com.javacp.MetacJava#overload(+2).",
      "com.javacp.MetacJava#overload(+1)."
    )
    assert(obtained == expected, info.toProtoString)
  }

}
