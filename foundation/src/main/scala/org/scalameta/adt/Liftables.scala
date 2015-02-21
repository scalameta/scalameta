package org.scalameta.adt

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import org.scalameta.unreachable
import org.scalameta.adt.Internal.Adt
import scala.{Seq => _}
import scala.collection.immutable.Seq

trait Liftables {
  val u: scala.reflect.macros.Universe
  implicit def materializeAdt[T <: Adt]: u.Liftable[T] = macro LiftableMacros.impl[T]
}

// TODO: copy/paste wrt org.scalameta.ast.LiftableMacros
class LiftableMacros(val c: Context) extends AdtReflection {
  val u: c.universe.type = c.universe
  import c.universe._
  def impl[T: WeakTypeTag]: c.Tree = {
    val root = weakTypeOf[T].typeSymbol.asAdt.root
    val leafs = weakTypeOf[T].typeSymbol.asAdt.root.allLeafs
    if (leafs.isEmpty) c.abort(c.enclosingPosition, s"$root hasn't been initialized properly")
    val u = q"${c.prefix}.u"
    val mainParam = c.freshName(TermName("x"))
    val mainModule = c.freshName(TermName("Module"))
    val mainMethod = c.freshName(TermName("instanceFor" + root.prefix.capitalize.replace(".", "")))
    val localParam = c.freshName(TermName("x"))
    val leafLiftNames = leafs.map(leaf => c.freshName(TermName("lift" + leaf.prefix.capitalize.replace(".", ""))))
    val liftLeafs = leafs.zip(leafLiftNames).map({ case (leaf, name) =>
      val init = q"""$u.Ident($u.TermName("_root_"))""": Tree
      val namePath = leaf.sym.fullName.split('.').foldLeft(init)((acc, part) => q"$u.Select($acc, $u.TermName($part))")
      val localParam = c.freshName(TermName("x"))
      val args = leaf.allFields.map(f => {
        val name = q"$u.Ident($u.TermName(${f.name.toString}))"
        val value = q"_root_.scala.Predef.implicitly[$u.Liftable[${f.tpe}]].apply($localParam.${f.name})"
        // NOTE: we can't really use AssignOrNamedArg here, sorry
        // Test.scala:10: warning: type-checking the invocation of method apply checks if the named argument expression 'stats = ...' is a valid assignment
        // in the current scope. The resulting type inference error (see above) can be fixed by providing an explicit type in the local definition for stats.
        // q"$u.AssignOrNamedArg($name, $value)"
        q"$value"
      })
      val body = if (leaf.sym.isClass) q"$u.Apply($namePath, $args)" else q"$namePath"
      q"def $name($localParam: ${leaf.tpe}): $u.Tree = $body"
    })
    val clauses = leafs.zip(leafLiftNames).map({ case (leaf, name) =>
      q"if ($localParam.isInstanceOf[${leaf.tpe}]) result = $name($localParam.asInstanceOf[${leaf.tpe}])"
    })
    q"""
      $u.Liftable(($mainParam: ${weakTypeOf[T]}) => {
        object $mainModule {
          val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
          ..$liftLeafs
          implicit def $mainMethod[T <: ${root.sym}]: $u.Liftable[T] = $u.Liftable(($localParam: T) => {
            var result: $u.Tree = null
            if ($localParam == null) result = q"null"
            ..$clauses
            if (result == null) sys.error("none of leafs matched " + $localParam.getClass)
            result
          })
        }
        $mainModule.$mainMethod.apply($mainParam)
      })
    """
  }
}