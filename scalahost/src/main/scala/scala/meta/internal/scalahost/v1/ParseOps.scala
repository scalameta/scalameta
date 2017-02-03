package scala.meta.internal
package scalahost
package v1

import scala.{meta => m}

trait ParseOps extends DialectOps with GlobalOps with ReflectionToolkit {

  implicit class XtensionCompilationUnitSource(unit: g.CompilationUnit) {
    def toSource: m.Source = {
      unit.cache.getOrElse("source", {
        val jfile = unit.source.file.file
        if (jfile == null)
          sys.error("Unsupported compilation unit with abstract file ${unit.source.file}")
        dialect(jfile).parse[m.Source].get
      })
    }
  }
}
