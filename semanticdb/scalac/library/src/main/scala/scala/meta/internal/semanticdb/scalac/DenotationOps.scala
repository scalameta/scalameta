package scala.meta.internal.semanticdb.scalac

import scala.{meta => m}
import scala.{meta => mf}
import scala.reflect.internal.{Flags => gf}
import scala.util.Sorting
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Accessibility.{Tag => a}
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}

trait DenotationOps { self: DatabaseOps =>
  import g._

  implicit class XtensionGSymbolMDenotation(gsym0: g.Symbol) {
    private val gsym: g.Symbol = {
      if (gsym0.isJavaClass) gsym0.companionClass
      else if (gsym0.isModuleClass) gsym0.asClass.module
      else if (gsym0.isTypeSkolem) gsym0.deSkolemize
      else gsym0
    }

    private val isObject = gsym.isModule && !gsym.hasFlag(gf.PACKAGE) && gsym.name != nme.PACKAGE

    private def kind: s.SymbolInformation.Kind = {
      gsym match {
        case _ if gsym.isSelfParameter => k.SELF_PARAMETER
        case gsym: MethodSymbol =>
          if (gsym.isConstructor) {
            k.CONSTRUCTOR
          } else {
            if (gsym.isGetter && gsym.isLazy && !gsym.isClass) {
              if (gsym.isLocalToBlock) k.LOCAL
              else k.FIELD
            } else if (gsym.isMacro) {
              k.MACRO
            } else {
              k.METHOD
            }
          }
        case gsym: ModuleSymbol =>
          if (gsym.hasPackageFlag) k.PACKAGE
          else if (gsym.isPackageObject) k.PACKAGE_OBJECT
          else k.OBJECT
        case gsym: TermSymbol =>
          if (gsym.isParameter) k.PARAMETER
          else if (gsym.isLocalToBlock) k.LOCAL
          else k.FIELD
        case gsym: ClassSymbol =>
          if (gsym.isTrait && gsym.hasFlag(gf.JAVA)) k.INTERFACE
          else if (gsym.isTrait) k.TRAIT
          else k.CLASS
        case gsym: TypeSymbol =>
          if (gsym.isParameter) k.TYPE_PARAMETER
          else k.TYPE
        case NoSymbol =>
          k.UNKNOWN_KIND
        case _ =>
          sys.error(s"unsupported symbol $gsym")
      }
    }

    private[meta] def propertyFlags: Long = {
      val kind = this.kind
      var flags = 0L
      def isAbstractClass =
        gsym.isClass && gsym.isAbstract && !gsym.isTrait && !gsym.hasFlag(gf.JAVA_ENUM)
      def isAbstractMethod = gsym.isMethod && gsym.isDeferred
      def isAbstractType = gsym.isType && !gsym.isParameter && gsym.isDeferred
      if (gsym.hasFlag(gf.PACKAGE)) {
        ()
      } else if (gsym.hasFlag(gf.JAVA)) {
        if (isAbstractClass || kind.isInterface || isAbstractMethod) flags |= mf.ABSTRACT
        if (gsym.hasFlag(gf.FINAL) || gsym.hasFlag(gf.JAVA_ENUM)) flags |= mf.FINAL
        if (gsym.hasFlag(gf.JAVA_ENUM)) flags |= mf.ENUM
        if (gsym.hasFlag(gf.STATIC)) flags |= mf.STATIC
        flags |= mf.JAVADEFINED
      } else {
        if (isAbstractClass || isAbstractMethod || isAbstractType) flags |= mf.ABSTRACT
        if (gsym.hasFlag(gf.FINAL) || isObject) flags |= mf.FINAL
        if (gsym.hasFlag(gf.SEALED)) flags |= mf.SEALED
        if (gsym.hasFlag(gf.IMPLICIT)) flags |= mf.IMPLICIT
        if (gsym.hasFlag(gf.LAZY)) flags |= mf.LAZY
        if (gsym.hasFlag(gf.CASE)) flags |= mf.CASE
        if (gsym.isType && gsym.hasFlag(gf.CONTRAVARIANT)) flags |= mf.CONTRAVARIANT
        if (gsym.isType && gsym.hasFlag(gf.COVARIANT)) flags |= mf.COVARIANT
        if (kind.isLocal || kind.isField) {
          if (gsym.isMutable) flags |= mf.VAR
          else flags |= mf.VAL
        }
        if (gsym.isGetter || gsym.isSetter) {
          if (gsym.isStable) flags |= mf.VAL
          else flags |= mf.VAR
        }
        if (gsym.isParameter && gsym.owner.isPrimaryConstructor) {
          val ggetter = gsym.getterIn(gsym.owner.owner)
          if (ggetter != g.NoSymbol && !ggetter.isStable) flags |= mf.VAR
          else if (ggetter != g.NoSymbol) flags |= mf.VAL
          else ()
        }
        if (gsym.isPrimaryConstructor) flags |= mf.PRIMARY
        if (gsym.isSemanticdbLocal)
          flags |= mf.LOCAL
      }
      flags
    }

    private def name: String = {
      gsym.name.toSemantic
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
      if (gsym.hasPackageFlag) (None, Nil)
      else {
        val ginfo = {
          if (gsym.isGetter && gsym.isLazy && !gsym.isClass) {
            gsym.info.finalResultType
          } else if (gsym.hasFlag(gf.JAVA_ENUM) && gsym.isStatic) {
            gsym.info.widen
          } else if (gsym.isAliasType) {
            def preprocess(info: g.Type): g.Type = {
              info match {
                case g.PolyType(tparams, tpe) => g.PolyType(tparams, preprocess(tpe))
                case tpe => g.TypeBounds(tpe, tpe)
              }
            }
            preprocess(gsym.info)
          } else {
            gsym.info
          }
        }
        ginfo.toSemantic
      }
    }

    private def overrides: List[m.Symbol] =
      if (config.overrides.isAll) gsym.overrides.map(_.toSemantic)
      else Nil

    private def anns: (List[s.Annotation], List[g.Symbol]) = {
      val buf = List.newBuilder[g.Symbol]
      val ganns = gsym.annotations.filter { gann =>
        gann.atp.typeSymbol != definitions.MacroImplAnnotation
      }
      val sanns = ganns.map { gann =>
        val (sann, todo) = gann.toSemantic
        todo.foreach(buf.+=)
        sann
      }
      (sanns, buf.result)
    }

    // TODO: I'm not completely happy with the implementation of this method.
    // See https://github.com/scalameta/scalameta/issues/1325 for details.
    private def acc: s.Accessibility = {
      if (gsym.hasFlag(gf.SYNTHETIC) && gsym.hasFlag(gf.ARTIFACT)) {
        // NOTE: some sick artifact vals produced by mkPatDef can be
        // private to method (whatever that means), so here we just ignore them.
        s.Accessibility(a.PUBLIC)
      } else {
        if (gsym.privateWithin == NoSymbol) {
          if (gsym.isPrivateThis) s.Accessibility(a.PRIVATE_THIS)
          else if (gsym.isPrivate) s.Accessibility(a.PRIVATE)
          else if (gsym.isProtectedThis) s.Accessibility(a.PROTECTED_THIS)
          else if (gsym.isProtected) s.Accessibility(a.PROTECTED)
          else s.Accessibility(a.PUBLIC)
        } else {
          val ssym = gsym.privateWithin.toSemantic.syntax
          if (gsym.isProtected) s.Accessibility(a.PROTECTED_WITHIN, ssym)
          else s.Accessibility(a.PRIVATE_WITHIN, ssym)
        }
      }
    }

    private def owner: m.Symbol = {
      if (config.owners.isAll && gsym.isSemanticdbGlobal) gsym.owner.toSemantic
      else m.Symbol.None
    }

    def toDenotation(saveOverrides: Boolean): DenotationResult = {
      val over = {
        if (saveOverrides) overrides
        else Nil
      }
      val todoOverrides = {
        if (saveOverrides && config.denotations.saveReferences) gsym.overrides
        else Nil
      }
      val (anns, todoAnns) = this.anns
      config.signatures match {
        case SignatureMode.None =>
          val denot = s.SymbolInformation()
          DenotationResult(denot, todoOverrides, todoAnns)
        case SignatureMode.New =>
          val (tpe, todoTpe) = newInfo
          val denot = s.SymbolInformation(
            symbol = gsym.toSemantic.syntax,
            language = s.Language.SCALA,
            kind = kind,
            accessibility = Some(acc),
            properties = ???,
            name = name,
            signature = None,
            members = Nil,
            overrides = Nil,
            tpe = tpe,
            annotations = anns,
            owner = owner.syntax
          )
          DenotationResult(denot, todoOverrides, todoAnns ++ todoTpe)
        case _ =>
          throw new UnsupportedOperationException(config.signatures.toString)
      }
    }
  }

  // NOTE: Holds a denotation along with todo lists of symbols to persist.
  case class DenotationResult(
      denot: s.SymbolInformation,
      todoOverrides: List[g.Symbol],
      todoTpe: List[g.Symbol])
}
