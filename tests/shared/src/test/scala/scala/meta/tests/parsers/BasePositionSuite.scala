package scala.meta.tests.parsers

import munit.FunSuite
import scala.meta.parsers.Parse
import scala.meta.internal.inputs._
import munit.TestOptions
import scala.meta.internal.trees.Origin
import munit.Location
import scala.meta.tests.parsers.MoreHelpers

abstract class BasePositionSuite extends ParseSuite {
  import scala.meta._
  def defaultDialect: Dialect = dialects.Scala213

  def check[T <: Tree: Parse](code: TestOptions)(implicit loc: Location): Unit = {
    check[T](code, "")
  }
  def check[T <: Tree: Parse](code: TestOptions, expected: String)(implicit loc: Location): Unit = {
    test(code) {
      implicit val D = defaultDialect
      val tree = MoreHelpers.requireNonEmptyOrigin(code.name.parse[T].get)
      val tokens = tree.collect {
        // Reduce the expected output by ignoring lines that can be trivially
        // verified. A line can be trivially verified when you can re-print the
        // `.syntax` without using tokens. For example, if a Mod.Lazy tree has
        // the syntax "lazy" then it's trivially verified and excluded from the
        // output.
        case t if t eq tree => Nil
        case t @ Lit(value) if t.syntax == value.toString =>
          Nil
        case t @ Lit.Unit() if t.syntax == "()" => // This case is needed for Scala.js.
          Nil
        case t @ Name(value) if t.syntax == value =>
          Nil
        case t @ Importee.Name(Name(value)) if t.syntax == value =>
          Nil
        case t @ Pat.Var(Name(value)) if t.syntax == value =>
          Nil
        case t: Mod if s"Mod.${t.syntax.capitalize}" == t.productPrefix =>
          Nil
        case t: Type.Param if t.syntax == t.name.value =>
          Nil
        case t @ Term.Param(Nil, name, Some(tpe), _) if t.syntax == s"${name}: ${tpe}" =>
          Nil
        case t @ Init(Type.Name(value), Name.Anonymous(), Nil) if t.syntax == value =>
          Nil
        case t: Importee.Wildcard if t.syntax == "_" =>
          Nil
        case t: Pat.Wildcard if t.syntax == "_" =>
          Nil
        case t =>
          val syntax = t.syntax
          val out = if (syntax.isEmpty) {
            val (leading, trailing) = t.pos.lineContent.splitAt(t.pos.startColumn)
            s"${t.productPrefix} ${leading}→←${trailing}"
          } else {
            s"${t.productPrefix} ${syntax}"
          }
          List(out)
      }
      val obtained = tokens.flatten.mkString("\n")
      assertNoDiff(obtained, expected)
    }
  }
}
