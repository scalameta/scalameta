package scala.meta.tests
package tokens

import org.scalatest._
import scala.compat.Platform.EOL
import scala.meta._
import scala.reflect.runtime.{universe => ru}
import scala.reflect.runtime.universe._

class ReflectionSuite extends FunSuite {
  object TokenReflection extends {
    val u: ru.type = ru
    val mirror: u.Mirror = u.runtimeMirror(classOf[scala.meta.tokens.Token].getClassLoader)
  } with scala.meta.internal.tokens.Reflection
  import TokenReflection._
  val tokens = symbolOf[scala.meta.tokens.Token].asRoot.allLeafs

  test("freeform tokens") {
    // tokens.filter(_.isFreeform).map(_.prefix).foreach(println)
    assert(tokens.filter(_.isFreeform).map(_.prefix).mkString(EOL) === """
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
      |Token.Interpolation.End
      |Token.Interpolation.Id
      |Token.Interpolation.Part
      |Token.Interpolation.SpliceEnd
      |Token.Interpolation.SpliceStart
      |Token.Interpolation.Start
      |Token.LFLF
      |Token.LeftArrow
      |Token.RightArrow
      |Token.Unquote
      |Token.Xml.End
      |Token.Xml.Part
      |Token.Xml.SpliceEnd
      |Token.Xml.SpliceStart
      |Token.Xml.Start
    """.trim.stripMargin)
  }
}