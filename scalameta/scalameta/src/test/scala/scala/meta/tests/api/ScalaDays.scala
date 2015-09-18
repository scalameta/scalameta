package scala.meta.tests
package api

import scala.meta._
import scala.meta.semantic.Context
import scala.meta.dialects.Scala211

object ScalaDays {
  def adtImpl(T: Type)(implicit c: Context) = {
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
          if (defn.isSealed) defn.submembers.foreach(validateLeaf)
          else abort(s"${defn.name} is not sealed")
        } else {
          abort(s"unsupported ref to ${defn.name}")
        }
        q"new Adt[$T]{}"
      case _ =>
        abort(s"unsupported type $T")
    }
  }

  def serializeImpl(T: Type)(implicit c: Context) = {
    T match {
      case ref: Type.Ref =>
        val defn = ref.defn
        if (defn.isClass || defn.isTrait || defn.isObject) {
          val serializer = Term.fresh(defn.name + "Serializer")
          val input = Term.fresh("input")
          val body = {
            def serializer(defn: Member, input: Term.Name, tagged: Boolean) = {
              val fields = defn.ctor.params.map(_.field)
              var entries: Seq[Term] = fields.map { field =>
                q""" "\"" + ${field.name.toString} + "\": " + serialize($input.${field.name}) """
              }
              if (tagged) {
                val tag = defn.supermembers.head.submembers.sortBy(_.name.toString).indexOf(defn).toString
                entries :+= q""" "$$tag: " + $tag """
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
              val clauses = defn.submembers.map(leaf => p"case $refined: ${leaf.tpe.pat} => ${serializer(leaf, refined.name, tagged = true)}")
              q"$input match { ..case $clauses }"
            } else {
              abort(s"unsupported ref to ${defn.name}")
            }
          }
          q"""
            implicit object $serializer extends Serializer[$T] {
              def serialize($input: $T): String = $body
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
}
