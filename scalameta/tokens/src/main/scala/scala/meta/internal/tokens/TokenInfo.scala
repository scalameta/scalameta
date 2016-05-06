package scala.meta
package internal
package tokens

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import java.lang.Class
import scala.meta.tokens.Token
import scala.meta.classifiers._
import org.scalameta.internal.MacroHelpers
import scala.meta.internal.tokens.{Reflection => TokenReflection}

@implicitNotFound(msg = "${T} is not a token class and can't be used here.")
trait TokenInfo[T <: Token] extends ClassTag[T] with Classifier[Token, T] {
  def name: String
  def runtimeClass: Class[T]
  def apply(token: Token): Boolean = token != null && runtimeClass.isAssignableFrom(token.getClass)
}
object TokenInfo {
  implicit def materialize[T <: Token]: TokenInfo[T] = macro TokenInfoMacros.materialize[T]
}

class TokenInfoMacros(val c: Context) extends MacroHelpers with TokenReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import u._
  def materialize[T](implicit T: c.WeakTypeTag[T]): c.Tree = {
    val TokenInfoClass = hygienicRef[scala.meta.internal.tokens.TokenInfo[_]]
    val sym = T.tpe.typeSymbol
    if (sym.isToken) {
      q"""
        new $TokenInfoClass[$T] {
          def name: $StringClass = ${sym.tokenName}
          def runtimeClass: $ClassClass[$T] = $ImplicitlyMethod[$ClassTagClass[$T]].runtimeClass.asInstanceOf[$ClassClass[$T]]
        }
      """
    } else {
      c.abort(c.enclosingPosition, s"${T.tpe} is not a token class and can't be used here.")
    }
  }
}