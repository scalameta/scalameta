package scala.meta.tests
package tokens

import org.scalameta.internal.ScalaCompat.EOL

import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

class ReflectionSuite extends TreeSuiteBase {
  object TokenReflection
      extends {
        val u: ru.type = ru
        val mirror: u.Mirror = u.runtimeMirror(classOf[scala.meta.tokens.Token].getClassLoader)
      }
      with scala.meta.internal.tokens.Reflection
  import TokenReflection._
  val tokens = symbolOf[scala.meta.tokens.Token].asRoot.allLeafs

  test("freeform tokens") {
    assertNoDiff(
      tokens.filter(_.isFreeform).map(_.prefix).sorted.mkString(EOL),
      """|
         |Token.BOF
         |Token.Comment
         |Token.Constant.Char
         |Token.Constant.Double
         |Token.Constant.Float
         |Token.Constant.Int
         |Token.Constant.Long
         |Token.Constant.String
         |Token.Constant.Symbol
         |Token.EOF
         |Token.Ellipsis
         |Token.Ident
         |Token.Indentation.Indent
         |Token.Indentation.Outdent
         |Token.InfixLF
         |Token.Interpolation.End
         |Token.Interpolation.Id
         |Token.Interpolation.Part
         |Token.Interpolation.SpliceEnd
         |Token.Interpolation.SpliceStart
         |Token.Interpolation.Start
         |Token.Invalid
         |Token.LFLF
         |Token.LeftArrow
         |Token.MultiHS
         |Token.MultiNL
         |Token.RightArrow
         |Token.Shebang
         |Token.Unquote
         |Token.Xml.End
         |Token.Xml.Part
         |Token.Xml.SpliceEnd
         |Token.Xml.SpliceStart
         |Token.Xml.Start
         |""".stripMargin.lf2nl
    )
  }
}
