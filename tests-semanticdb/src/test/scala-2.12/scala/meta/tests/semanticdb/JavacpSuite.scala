package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.Scala._

class JavacpSuite extends JavacpSuiteBase {

  checkOrder(
    "methods",
    "com/javacp/MetacJava#",
    _.desc.value == "overload",
    List(
      "com/javacp/MetacJava#overload().",
      "com/javacp/MetacJava#overload(+2).",
      "com/javacp/MetacJava#overload(+1)."
    )
  )

  checkOrder(
    "fields",
    "com/javacp/Test#",
    s => s.desc.value == "Int" || s.desc.value == "Long" || s.desc.value == "Float",
    List("com/javacp/Test#Int.", "com/javacp/Test#Long.", "com/javacp/Test#Float.")
  )

}
