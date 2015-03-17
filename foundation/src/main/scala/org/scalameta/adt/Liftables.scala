package org.scalameta.adt

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
  def impl[T: WeakTypeTag]: c.Tree = {
    val root = weakTypeOf[T].typeSymbol.asAdt.root
    val leafs = weakTypeOf[T].typeSymbol.asAdt.root.allLeafs.sortWith(_.tpe <:< _.tpe)
    if (leafs.isEmpty) c.abort(c.enclosingPosition, s"$root has no known leafs")
    val u = q"${c.prefix}.u"
    val mainParam = c.freshName(TermName("x"))
    val mainModule = c.freshName(TermName("Module"))
    val mainMethod = TermName("liftableSub" + root.prefix.capitalize.replace(".", ""))
    val localParam = c.freshName(TermName("x"))
    val leafLiftNames = leafs.map(leaf => c.freshName(TermName("lift" + leaf.prefix.capitalize.replace(".", ""))))
    val liftLeafs = leafs.zip(leafLiftNames).map({ case (leaf, name) =>
      // TODO: it should be possible to customize liftable codegen by providing implicit instances on the outside
      // we can't just do `inferImplicitValue(leaf.tpe)`, because that'll lead to a stack overflow
      // we need to do something pickling-like, but I just don't have time to implement that right now
      if (leaf.sym.fullName == "scala.meta.internal.ast.Ellipsis") {
        q"def $name($localParam: ${leaf.tpe}): $u.Tree = liftEllipsis.apply($localParam)"
      } else if (leaf.sym.fullName == "scala.meta.internal.ast.Unquote") {
        q"def $name($localParam: ${leaf.tpe}): $u.Tree = liftUnquote.apply($localParam)"
      } else if (leaf.tpe <:< c.mirror.staticClass("scala.meta.internal.ast.Name").toType) {
        q"def $name($localParam: ${leaf.tpe}): $u.Tree = liftName.apply($localParam)"
      } else {
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
      }
    })
    val clauses = leafs.zip(leafLiftNames).map({ case (leaf, name) =>
      cq"$localParam: ${leaf.tpe} => $name($localParam.asInstanceOf[${leaf.tpe}])"
    })
    val result = q"""
      $u.Liftable(($mainParam: ${weakTypeOf[T]}) => {
        object $mainModule {
          val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
          ..$liftLeafs
          implicit def $mainMethod[T <: ${root.sym}]: $u.Liftable[T] = $u.Liftable(($localParam: T) => {
            $localParam match {
              case ..$clauses
              case _ => sys.error("none of leafs matched " + $localParam.getClass)
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