package org.scalameta.adt

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import org.scalameta.unreachable
import org.scalameta.adt.Internal.Adt
import org.scalameta.adt.{Reflection => AdtReflection}

trait Liftables {
  val u: scala.reflect.macros.Universe
  implicit def materializeAdt[T <: Adt]: u.Liftable[T] = macro LiftableMacros.impl[T]
}

class LiftableMacros(val c: Context) extends AdtReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.universe._
  def customAdts(root: Root): Option[List[Adt]] = None
  def customMatcher(adt: Adt, defName: TermName, localName: TermName): Option[DefDef] = None
  def impl[T: WeakTypeTag]: c.Tree = {
    val root = weakTypeOf[T].typeSymbol.asAdt.root
    val unsortedAdts = customAdts(root).getOrElse(weakTypeOf[T].typeSymbol.asAdt.root.allLeafs)
    val adts = {
      // TODO: This doesn't quite work, because we can have `A` and `B`, none of which inherits each other,
      // but then at runtime we get `C` which inherits both and then execution suddenly takes the wrong path.
      // Real life example: Term.Name and Quasi.Unquote, none of them are related, so we kinda can reorder their cases, right?
      // Nope! If we get Term.Name.Unquote, then we really need it to go into Quasi.Unquote, but not into Term.Name.
      // Therefore, simple sorting doesn't work.
      // // NOTE: need to make sure that more specific leafs come before less specific ones
      // // so that we don't get dead code during pattern matching
      // val cache = mutable.Map[Symbol, Int]()
      // def metric(sym: Symbol): Int = cache.getOrElseUpdate(sym, {
      //   if (sym == root.sym) 0
      //   else {
      //     val classSym = if (sym.isModule) sym.asModule.moduleClass else sym.asClass
      //     val parents = classSym.info.asInstanceOf[ClassInfoType].parents.map(_.typeSymbol)
      //     val relevantParents = parents.filter(_.asType.toType <:< root.tpe)
      //     relevantParents.length + relevantParents.map(metric).sum
      //   }
      // })
      // unsortedAdts.sortBy(adt => -1 * metric(adt.sym))
      unsortedAdts
    }
    if (adts.isEmpty) c.abort(c.enclosingPosition, s"$root has no known leafs")
    val u = q"${c.prefix}.u"
    val mainParam = c.freshName(TermName("x"))
    val mainModule = c.freshName(TermName("Module"))
    val mainMethod = TermName("liftableSub" + root.prefix.capitalize.replace(".", ""))
    val localName = c.freshName(TermName("x"))
    val defNames = adts.map(adt => c.freshName(TermName("lift" + adt.prefix.capitalize.replace(".", ""))))
    val liftAdts = adts.zip(defNames).map{ case (adt, defName) =>
      customMatcher(adt, defName, localName).getOrElse({
        val init = q"""$u.Ident($u.TermName("_root_"))""": Tree
        val namePath = adt.sym.fullName.split('.').foldLeft(init)((acc, part) => q"$u.Select($acc, $u.TermName($part))")
        val fields = adt match { case leaf: Leaf => leaf.allFields; case _ => sys.error(s"fatal error: $adt") }
        val args = fields.map(f => {
          val fieldName = q"$u.Ident($u.TermName(${f.name.toString}))"
          val fieldValue = q"_root_.scala.Predef.implicitly[$u.Liftable[${f.tpe}]].apply($localName.${f.name})"
          // NOTE: we can't really use AssignOrNamedArg here, sorry
          // Test.scala:10: warning: type-checking the invocation of method apply checks if the named argument expression 'stats = ...' is a valid assignment
          // in the current scope. The resulting type inference error (see above) can be fixed by providing an explicit type in the local definition for stats.
          // q"$u.AssignOrNamedArg($fieldName, $fieldValue)"
          q"$fieldValue"
        })
        val body = if (adt.sym.isClass) q"$u.Apply($namePath, $args)" else q"$namePath"
        q"def $defName($localName: ${adt.tpe}): $u.Tree = $body"
      })
    }
    val clauses = adts.zip(defNames).map({ case (adt, name) =>
      cq"$localName: ${adt.tpe} => $name($localName.asInstanceOf[${adt.tpe}])"
    })
    val result = q"""
      $u.Liftable(($mainParam: ${weakTypeOf[T]}) => {
        object $mainModule {
          val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
          ..$liftAdts
          implicit def $mainMethod[T <: ${root.sym}]: $u.Liftable[T] = $u.Liftable(($localName: T) => {
            $localName match {
              case ..$clauses
              case _ => sys.error("none of leafs matched " + $localName.getClass)
            }
          })
        }
        $mainModule.$mainMethod.apply($mainParam)
      })
    """
    if (sys.props("adt.debug") != null || sys.props("ast.debug") != null) println(result)
    result
  }
}