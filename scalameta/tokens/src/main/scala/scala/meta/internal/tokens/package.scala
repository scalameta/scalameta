package scala.meta
package internal

import scala.meta.tokens.Token

package object tokens {
  implicit class XtensionTokenName(token: Token) {
    def name: String = {
      import scala.reflect.runtime.{universe => ru}
      object TokenReflection extends {
        val u: ru.type = ru
        val mirror: u.Mirror = u.runtimeMirror(classOf[Token].getClassLoader)
      } with scala.meta.internal.tokens.Reflection
      import TokenReflection._
      val mirror = ru.runtimeMirror(classOf[Token].getClassLoader)
      mirror.classSymbol(token.getClass).asLeaf.tokenName
    }
  }
}
