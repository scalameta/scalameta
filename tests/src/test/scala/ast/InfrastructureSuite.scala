package scala.meta

import org.scalatest._
import scala.meta._
import scala.meta.syntactic.quasiquotes._
import scala.meta.semantic._
import scala.meta.syntactic._
import scala.meta.internal.hygiene.Symbol
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.dialects.Scala211

class InfrastructureSuite extends FunSuite {
  test("appendScratchpad") {
    val tree = q"foo.bar"
    tree.toString // check well-formedness
    implicit object FooContext extends Context {
      def dialect: Dialect = Scala211
      private[meta] def tpe(term: Term): Type = ???
      private[meta] def defns(ref: Ref): Seq[Member] = ???
      private[meta] def members(tpe: Type): Seq[Member] = ???
      private[meta] def isSubType(tpe1: Type, tpe2: Type): Boolean = ???
      private[meta] def lub(tpes: Seq[Type]): Type = ???
      private[meta] def glb(tpes: Seq[Type]): Type = ???
      private[meta] def parents(member: Member): Seq[Member] = ???
      private[meta] def children(member: Member): Seq[Member] = ???
    }
    val tree1 = tree.appendScratchpad(tree)
    tree1.toString // check well-formedness again
    assert(tree1.scratchpad.length === 1)
    assert(tree1.scratchpad.head === tree)
  }
}
