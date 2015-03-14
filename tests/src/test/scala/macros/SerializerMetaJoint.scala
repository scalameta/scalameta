package serializer.meta.joint

import scala.language.experimental.macros
import scala.meta._
import scala.meta.dialects.Scala211

package adt {
  trait Adt[T]
  object Adt {
    implicit def materialize[T]: Adt[T] = macro {
      T match {
        case ref: Type.Ref =>
          def validateLeaf(leaf: Member) = {
            if (!leaf.isFinal) abort(s"${leaf.name} is not final")
            if (!leaf.isCase) abort(s"${leaf.name} is not sealed")
            if (!leaf.tparams.isEmpty) abort(s"${leaf.name} is not monomorphic")
          }
          val defn = ref.defn
          if (defn.isClass || defn.isObject) {
            validateLeaf(defn)
          } else if (defn.isTrait) {
            if (defn.isSealed) defn.children.foreach(validateLeaf)
            else abort(s"${defn.name} is not sealed")
          } else {
            abort(s"unsupported ref to ${defn.name}")
          }
          q"new Adt[$T]{}"
        case _ =>
          abort(s"unsupported type $T")
      }
    }
  }
}

package serialization {
  trait Serializer[T] {
    def apply(x: T): String
  }

  object Serializer {
    implicit def materialize[T: adt.Adt]: Serializer[T] = macro {
      T match {
        case ref: Type.Ref =>
          val defn = ref.defn
          if (defn.isClass || defn.isTrait || defn.isObject) {
            val serializer = Term.fresh(defn.name + "Serializer")
            val input = Term.fresh("input")
            val body = {
              def serializer(defn: Member, input: Term.Name, tagged: Boolean) = {
                val fields = defn.ctor.params.map(x => x.field)
                var entries: Seq[Term] = fields.map { field =>
                  q""" "\"" + ${field.name.toString} + "\": " + serialize($input.${field.name}) """
                }
                if (tagged) {
                  val tag = defn.parents.head.children.indexOf(defn).toString
                  entries = entries :+ q""" "$$tag: " + $tag """
                }
                val unwrappedResult = entries.foldLeft(None: Option[Term]) { (acc, curr) =>
                  acc.map(acc => q"""$acc + ", " + $curr""").orElse(Some(curr))
                }
                val contents = unwrappedResult.getOrElse(q""" "" """)
                q""" "{" + $contents + "}" """
              }
              if (defn.isClass) {
                serializer(defn, input, tagged = false)
              } else if (defn.isObject) {
                serializer(defn, input, tagged = false)
              } else if (defn.isTrait) {
                val refined = Pat.fresh("input")
                val clauses = defn.children.map(leaf => p"case $refined: ${leaf.tpe.pat} => ${serializer(leaf, refined.name, tagged = true)}")
                q"$input match { ..$clauses }"
              } else {
                abort(s"unsupported ref to ${defn.name}")
              }
            }
            q"""
              implicit object $serializer extends Serializer[$T] {
                def apply($input: $T): String = $body
              }
              $serializer
            """
          } else {
            abort(s"unsupported ref to ${defn.name}")
          }
        case _ =>
          abort(s"unsupported type $T")
      }
    }
    implicit def intSerializer: Serializer[Int] = new Serializer[Int] { def apply(x: Int) = x.toString }
    implicit def stringSerializer: Serializer[String] = new Serializer[String] { def apply(x: String) = x }
  }

  object serialize {
    def apply[T](x: T)(implicit ev: Serializer[T]) = ev(x)
  }
}

import org.scalatest._
import serializer.meta.joint.adt._
import serializer.meta.joint.serialization._

sealed trait List
final case class Cons(head: Int, tail: List) extends List
final case object Nil extends List

trait SerializeMetaJointSuite extends FunSuite {
  // NOTE: In memory of: https://github.com/scalareflect/core/commit/7f3058d1293b46fc255f525a92deaeec4d64026d
  // Initial commit
  // master
  //  scalareflect  authored on Mar 8, 2014
  test("San Francisco 2015 (joint compilation)") {
    val list: List = Cons(1, Cons(2, Nil))
    assert(serialize(list) === """{"head": 1, "tail": {"head": 2, "tail": {$tag: 1}, $tag: 0}, $tag: 0}""")
  }
}