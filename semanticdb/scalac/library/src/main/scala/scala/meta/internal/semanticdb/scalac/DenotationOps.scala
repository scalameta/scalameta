package scala.meta.internal.semanticdb.scalac

import scala.{meta => m}
import scala.{meta => mf}
import scala.reflect.internal.{Flags => gf}
import scala.util.Sorting
import org.scalameta.logger
import scala.meta.internal.{semanticdb3 => s}

trait DenotationOps { self: DatabaseOps =>
  import g._

  implicit class XtensionGSymbolMDenotation(gsym0: g.Symbol) {
    private val gsym: g.Symbol = {
      if (gsym0.isModuleClass) gsym0.asClass.module
      else if (gsym0.isTypeSkolem) gsym0.deSkolemize
      else gsym0.setterIn(gsym0.owner).orElse(gsym0.getterIn(gsym0.owner).orElse(gsym0))
    }

    private val isObject = gsym.isModule && !gsym.hasFlag(gf.PACKAGE) && gsym.name != nme.PACKAGE

    private def definitionFlags: Long = {
      var flags = 0l
      def maybeValOrVar =
        (gsym.isTerm && flags == 0l) || (gsym.hasFlag(gf.PARAMACCESSOR) && flags == mf.PARAM)
      if (gsym.isMethod && !gsym.isConstructor && !gsym.hasFlag(gf.MACRO) && !gsym.hasFlag(
            gf.ACCESSOR) && !gsym.hasFlag(gf.PARAMACCESSOR)) flags |= mf.DEF
      if (gsym.isPrimaryConstructor) flags |= mf.PRIMARYCTOR
      if (gsym.isConstructor && !gsym.isPrimaryConstructor) flags |= mf.SECONDARYCTOR
      if (gsym.hasFlag(gf.MACRO)) flags |= mf.MACRO
      if (gsym.isType && !gsym.isClass && !gsym.hasFlag(gf.PARAM)) flags |= mf.TYPE
      if (gsym.isTerm && (gsym.hasFlag(gf.PARAM) || gsym.hasFlag(gf.PARAMACCESSOR)))
        flags |= mf.PARAM
      if (gsym.isType && gsym.hasFlag(gf.PARAM)) flags |= mf.TYPEPARAM
      if (isObject) flags |= mf.OBJECT
      if (gsym.hasFlag(gf.PACKAGE)) flags |= mf.PACKAGE
      if (gsym.isModule && gsym.name == nme.PACKAGE) flags |= mf.PACKAGEOBJECT
      if (gsym.isClass && !gsym.hasFlag(gf.TRAIT)) flags |= mf.CLASS
      if (gsym.isClass && gsym.hasFlag(gf.TRAIT)) flags |= mf.TRAIT
      if (maybeValOrVar && (gsym.hasFlag(gf.MUTABLE) || nme.isSetterName(gsym.name)))
        flags |= mf.VAR
      if (maybeValOrVar && !(gsym.hasFlag(gf.LOCAL) && gsym.hasFlag(gf.PARAMACCESSOR)))
        flags |= mf.VAL
      if (gsym.hasFlag(gf.JAVA) && !gsym.hasFlag(gf.PACKAGE))
        flags |= mf.JAVADEFINED
      flags
    }

    private def accessQualifierFlags: Long = {
      var flags = 0l
      val gpriv = gsym.privateWithin.orElse(gsym.owner)
      if (gsym.hasFlag(gf.SYNTHETIC) && gsym.hasFlag(gf.ARTIFACT)) {
        // NOTE: some sick artifact vals produced by mkPatDef can be private to method (whatever that means)
        // so here we just ignore them
      } else {
        if (gsym.hasFlag(gf.PROTECTED)) flags |= mf.PROTECTED
        if (gsym.hasFlag(gf.PRIVATE) && !gsym.hasFlag(gf.PARAMACCESSOR)) flags |= mf.PRIVATE
        // TODO: `private[pkg] class C` doesn't have PRIVATE in its flags
        // so we need to account for that!
        if (gsym.hasAccessBoundary && gpriv != g.NoSymbol && !gsym.hasFlag(gf.PROTECTED))
          flags |= mf.PRIVATE
      }
      flags
    }

    private def otherFlags: Long = {
      var flags = 0l
      val isDeclaredDeferred = gsym.hasFlag(gf.DEFERRED) && !gsym.hasFlag(gf.PARAM)
      val isDeclaredAbstract = (gsym.hasFlag(gf.ABSTRACT) && !gsym.hasFlag(gf.TRAIT)) || gsym
        .hasFlag(gf.ABSOVERRIDE)
      if (isDeclaredDeferred || isDeclaredAbstract) flags |= mf.ABSTRACT
      if ((gsym.hasFlag(gf.FINAL) && !gsym.hasFlag(gf.PACKAGE)) || isObject) flags |= mf.FINAL
      if (gsym.hasFlag(gf.SEALED)) flags |= mf.SEALED
      if (gsym.hasFlag(gf.IMPLICIT)) flags |= mf.IMPLICIT
      if (gsym.hasFlag(gf.LAZY)) flags |= mf.LAZY
      if (gsym.hasFlag(gf.CASE)) flags |= mf.CASE
      if (gsym.isType && gsym.hasFlag(gf.CONTRAVARIANT)) flags |= mf.CONTRAVARIANT
      if (gsym.isType && gsym.hasFlag(gf.COVARIANT)) flags |= mf.COVARIANT
      // TODO: mf.INLINE
      flags
    }

    private def flags: Long = {
      definitionFlags | accessQualifierFlags | otherFlags
    }

    private def name: String = {
      gsym.decodedName.toString
    }

    private def oldInfo: (String, List[m.ResolvedName]) = {
      if (gsym.isClass || gsym.isModule) "" -> Nil
      else {
        val synthetic = showSynthetic(gsym.info)
        val input = m.Input.Denotation(synthetic.text, gsym.toSemantic)
        val names = synthetic.names.toIterator.map {
          case SyntheticRange(start, end, syntheticSymbol) =>
            m.ResolvedName(
              m.Position.Range(input, start, end),
              syntheticSymbol,
              isDefinition = false)
        }.toArray
        Sorting.quickSort(names)(Ordering.by[m.ResolvedName, Int](_.position.start))
        synthetic.text -> names.toList
      }
    }

    private def newInfo: (Option[s.Type], List[g.Symbol]) = {
      gsym.info.toSemantic
    }

    private def overrides: List[m.Symbol] =
      if (config.overrides.isAll) gsym.overrides.map(_.toSemantic)
      else Nil

    def toDenotation(saveOverrides: Boolean): DenotationResult = {
      val over = {
        if (saveOverrides) overrides
        else Nil
      }
      val todoOverrides = {
        if (saveOverrides && config.denotations.saveReferences) gsym.overrides
        else Nil
      }
      config.signatures match {
        case SignatureMode.None =>
          val denot = m.Denotation(flags, name, "", Nil, Nil, over, None)
          DenotationResult(denot, todoOverrides, Nil)
        case SignatureMode.Old =>
          val (signature, names) = oldInfo
          val denot = m.Denotation(flags, name, signature, names, Nil, over, None)
          DenotationResult(denot, todoOverrides, Nil)
        case SignatureMode.New =>
          val (tpe, todoTpe) = newInfo
          val denot = m.Denotation(flags, name, "", Nil, Nil, over, tpe)
          DenotationResult(denot, todoOverrides, todoTpe)
        case SignatureMode.All =>
          val (signature, names) = oldInfo
          val (tpe, todoTpe) = newInfo
          val denot = m.Denotation(flags, name, signature, names, Nil, over, tpe)
          DenotationResult(denot, todoOverrides, todoTpe)
      }
    }
  }

  // NOTE: Holds a denotation along with todo lists of symbols to persist.
  case class DenotationResult(
      denot: m.Denotation,
      todoOverrides: List[g.Symbol],
      todoTpe: List[g.Symbol])
}
