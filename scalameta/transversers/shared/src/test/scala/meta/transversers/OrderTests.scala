package scala.meta.transversers

import scala.meta._
import org.scalatest._
import scala.meta.quasiquotes._
import scala.collection.mutable

/**
 * Note: Traversal is not exactly pre/post/in order.
 * The children aare sorted. For most cases we only
 * care about leaf first or parent first
 */
class OrderTests extends FunSuite {

  val rootFirst = mutable.ArrayBuffer(
    "scala.meta.Defn$Class$DefnClassImpl",
    "scala.meta.Type$Name$TypeNameImpl",
    "scala.meta.Ctor$Primary$CtorPrimaryImpl",
    "scala.meta.Name$Anonymous$NameAnonymousImpl",
    "scala.meta.Template$TemplateImpl",
    "scala.meta.Self$SelfImpl",
    "scala.meta.Name$Anonymous$NameAnonymousImpl",
    "scala.meta.Defn$Def$DefnDefImpl",
    "scala.meta.Term$Name$TermNameImpl",
    "scala.meta.Lit$Int$LitIntImpl")

    val leafFirst = mutable.ArrayBuffer(
      "scala.meta.Type$Name$TypeNameImpl",
      "scala.meta.Name$Anonymous$NameAnonymousImpl",
      "scala.meta.Ctor$Primary$CtorPrimaryImpl",
      "scala.meta.Name$Anonymous$NameAnonymousImpl",
      "scala.meta.Self$SelfImpl",
      "scala.meta.Term$Name$TermNameImpl",
      "scala.meta.Lit$Int$LitIntImpl",
      "scala.meta.Defn$Def$DefnDefImpl",
      "scala.meta.Template$TemplateImpl",
      "scala.meta.Defn$Class$DefnClassImpl")

  val input = q"class Foo { def bar = 1 }"

  test("Traverse visits roots first") {
    val visited: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

    input.traverse {
      case a => visited.append(a.getClass.getName)
    }

    assert(visited === rootFirst)
  }

  test("Transform visits roots first") {
    val visited: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

    input.transform {
      case a =>
        visited.append(a.getClass.getName)
        a
    }

    assert(visited === rootFirst)
  }

  test("Collect visits roots first") {
    val visited: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

    input.collect {
      case a =>
        visited.append(a.getClass.getName)
        a
    }

    assert(visited === rootFirst)
  }

  test("Leaf first transform visits leafs first") {
    val visited: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

    input.transform({
      case a =>
        visited.append(a.getClass.getName)
        a
    })(LeafFirst)

    assert(visited === leafFirst)
  }

  test("Leaf first traverse visits leafs first") {
    val visited: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

    input.traverse({
      case a =>
        visited.append(a.getClass.getName)
    })(LeafFirst)

    assert(visited === leafFirst)
  }

  test("Leaf first collect visits leafs first") {
    val visited: mutable.ArrayBuffer[String] = mutable.ArrayBuffer()

    input.collect({
      case a =>
        visited.append(a.getClass.getName)
        a
    })(LeafFirst)

    assert(visited === leafFirst)
  }
}
