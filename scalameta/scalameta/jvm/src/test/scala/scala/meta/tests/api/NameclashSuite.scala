package scala.meta.tests
package api

import org.scalatest._

class NameclashSuite extends FunSuite {
  import scala.reflect.runtime.{universe => ru}
  object TreeReflection extends {
    val u: ru.type = ru
    val mirror: u.Mirror = u.runtimeMirror(classOf[scala.meta.Tree].getClassLoader)
  } with scala.meta.internal.ast.Reflection
  object TokenReflection extends {
    val u: ru.type = ru
    val mirror: u.Mirror = u.runtimeMirror(classOf[scala.meta.tokens.Token].getClassLoader)
  } with scala.meta.internal.tokens.Reflection
  val TreeRoot = { import TreeReflection._; ru.symbolOf[scala.meta.Tree].asRoot }
  val TokenRoot = { import TokenReflection._; ru.symbolOf[scala.meta.tokens.Token].asRoot }

  test("name clashes between top-level trees and tokens") {
    val tlTrees = TreeRoot.all.map(_.prefix)
    val tlTokens = TokenRoot.all.map(_.prefix).map(_.stripPrefix("Token."))
    val nameClashes = tlTrees.intersect(tlTokens).sorted
    assert(nameClashes.mkString(", ") === "")
  }
}
