package scala.meta

import org.scalatest._
import scala.meta._
import scala.meta.semantic._
import scala.meta.syntactic.parsers._
import scala.{Seq => _}
import scala.collection.immutable.Seq

class InfrastructureSuite extends FunSuite {
  test("appendScratchpad") {
    val tree = q"foo.bar"
    tree.toString // check well-formedness
    implicit object FooHost extends Host {
      def defns(ref: Ref): Seq[Tree] = ???
      def attrs(tree: Tree): Seq[Attr] = ???
      def owner(tree: Tree): Scope = ???
      def members(scope: Scope): Seq[Tree] = ???
      def members(scope: Scope, name: Name): Seq[Tree] = ???
      def <:<(tpe1: Type, tpe2: Type): Boolean = ???
      def lub(tpes: Seq[Type]): Type = ???
      def glb(tpes: Seq[Type]): Type = ???
      def superclasses(member: Member.Template): Seq[Member.Template] = ???
      def subclasses(member: Member.Template): Seq[Member.Template] = ???
      def overridden(member: Member): Seq[Member] = ???
      def overriding(member: Member): Seq[Member] = ???
      def erasure(tpe: Type): Type = ???
    }
    val tree1 = tree.appendScratchpad(tree)
    tree1.toString // check well-formedness again
  }
}
