package org.scalameta.adt

import org.scalameta.adt.Metadata.Adt
import org.scalameta.adt.{Reflection => AdtReflection}
import scala.meta.internal.trees.AstNamerMacros

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.blackbox.Context

// Implementation of the scala.reflect.api.Universe#Liftable interface for adts.
trait Liftables {
  val u: scala.reflect.macros.Universe
  implicit def materializeAdt[T <: Adt](isPrivateOKExpr: Boolean): u.Liftable[T] =
    macro LiftableMacros.impl[T]
}

class LiftableMacros(val c: Context) extends AdtReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.universe._
  def customAdts(root: Root): Option[List[Adt]] = None
  def customMatcher(adt: Adt, defName: TermName, localName: TermName): Option[DefDef] = None
  def customWrapper(adt: Adt, defName: TermName, localName: TermName, body: Tree): Option[Tree] =
    None
  def impl[T: WeakTypeTag](isPrivateOKExpr: c.Expr[Boolean]): c.Tree = {
    val isPrivateOK = c.eval(isPrivateOKExpr)
    val root = weakTypeOf[T].typeSymbol.asAdt.root
    val unsortedAdts = customAdts(root).getOrElse(root.allLeafs)
    val adts =
      // NOTE: The code below doesn't quite work, because we can have `A` and `B`, none of which inherits each other,
      // but then at runtime we get `C` which inherits both and then execution suddenly takes the wrong path.
      // Real life example: Term.Name and Quasi, none of them are related, so we kinda can reorder their cases, right?
      // Nope! If we get Term.Name.Quasi, then we really need it to go into Quasi, but not into Term.Name.
      // Therefore, simple sorting doesn't work.
      //
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
    if (adts.isEmpty) {
      val message = s"materialization failed for Liftable[${weakTypeOf[T]}] " +
        s"(the most common reason for that is that you cannot materialize ADTs that haven't been compiled yet, " +
        s"i.e. materialization will fail if the file with ADT definitions comes after the file with the materialization call)"
      c.abort(c.enclosingPosition, message)
    }
    val u = q"${c.prefix}.u"
    val mainParam = c.freshName(TermName("x"))
    val mainModule = c.freshName(TermName("Module"))
    val mainMethod = TermName("liftableSub" + root.prefix.capitalize.replace(".", ""))
    val localName = c.freshName(TermName("x"))
    val defNames = adts
      .map(adt => c.freshName(TermName("lift" + adt.prefix.capitalize.replace(".", ""))))
    val liftAdts = adts.zip(defNames).map { case (adt, defName) =>
      val matcher: DefDef = customMatcher(adt, defName, localName).getOrElse {
        val init = q"""$u.Ident($u.TermName("_root_"))""": Tree
        def getNamePath(parts: Iterable[String]): Tree = parts.foldLeft(init) { (acc, part) =>
          q"$u.Select($acc, $u.TermName($part))"
        }
        val nameParts = adt.sym.fullName.split('.')
        val body =
          if (adt.sym.isClass) {
            val fields = adt match { case leaf: Leaf => leaf.fields(isPrivateOK); case _ => Nil }
            val args = fields.map { f =>
              q"_root_.scala.Predef.implicitly[$u.Liftable[${f.tpe}]].apply($localName.${f.name})"
              // NOTE: we can't really use AssignOrNamedArg here, sorry
              // Test.scala:10: warning: type-checking the invocation of method apply checks if the named argument expression 'stats = ...' is a valid assignment
              // in the current scope. The resulting type inference error (see above) can be fixed by providing an explicit type in the local definition for stats.
              // q"$u.AssignOrNamedArg($fieldName, $fieldValue)"
            }
            val latestAfterVersion =
              if (adt.sym.isAstClass) {
                val moduleNames = adt.sym.companion.info.decls.flatMap { x =>
                  if (x.isModule) Some(x.name.toString) else None
                }
                val latestAfterVersion = AstNamerMacros.getLatestAfterName(moduleNames).getOrElse {
                  c.abort(
                    c.enclosingPosition,
                    s"no latest version ${adt.sym.fullName}: $moduleNames"
                  )
                }
                latestAfterVersion :: Nil
              } else Nil
            val namePath = getNamePath(nameParts ++ latestAfterVersion)
            q"$u.Apply($namePath, $args)"
          } else getNamePath(nameParts)
        q"def $defName($localName: ${adt.tpe}): $u.Tree = $body"
      }
      val body: Tree = customWrapper(adt, defName, localName, matcher.rhs).getOrElse(matcher.rhs)
      treeCopy.DefDef(
        matcher,
        matcher.mods,
        matcher.name,
        matcher.tparams,
        matcher.vparamss,
        matcher.tpt,
        body
      )
    }
    val clauses = adts.zip(defNames).map { case (adt, name) =>
      cq"$localName: ${adt.tpe} => $name($localName.asInstanceOf[${adt.tpe}])"
    }
    q"""
      $u.Liftable(($mainParam: ${weakTypeOf[T]}) => {
        object $mainModule {
          val XtensionQuasiquoteTerm = "shadow scala.meta quasiquotes"
          ..$liftAdts
          implicit def $mainMethod[T <: ${root.sym}]: $u.Liftable[T] = $u.Liftable(($localName: T) => {
            $localName match {
              case ..$clauses
              case null => null
              case _ => sys.error("none of leafs matched " + $localName.getClass)
            }
          })
        }
        $mainModule.$mainMethod.apply($mainParam)
      })
    """
  }
}
