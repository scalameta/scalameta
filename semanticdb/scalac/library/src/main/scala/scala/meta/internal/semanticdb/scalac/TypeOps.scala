package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.LiteralType.{Tag => l}
import scala.meta.internal.semanticdb3.Type.{Tag => t}

trait TypeOps { self: DatabaseOps =>
  implicit class XtensionGTypeSType(gtpe: g.Type) {
    def toSemantic: (Option[s.Type], List[g.Symbol]) = {
      val buf = List.newBuilder[g.Symbol]
      def todo(gsym: g.Symbol): String = {
        buf += gsym
        gsym.toSemantic.syntax
      }
      def loop(gtpe: g.Type): Option[s.Type] = {
        gtpe match {
          case ByNameType(gtpe) =>
            val stag = t.BY_NAME_TYPE
            val stpe = loop(gtpe)
            Some(s.Type(tag = stag, byNameType = Some(s.ByNameType(stpe))))
          case RepeatedType(gtpe) =>
            val stag = t.REPEATED_TYPE
            val stpe = loop(gtpe)
            Some(s.Type(tag = stag, repeatedType = Some(s.RepeatedType(stpe))))
          case g.TypeRef(gpre, gsym, gargs) =>
            val stag = t.TYPE_REF
            val spre = if (gtpe.hasNontrivialPrefix) loop(gpre) else None
            val ssym = todo(gsym)
            val sargs = gargs.flatMap(loop)
            Some(s.Type(tag = stag, typeRef = Some(s.TypeRef(spre, ssym, sargs))))
          case g.SingleType(gpre, gsym) =>
            val stag = t.SINGLE_TYPE
            val spre = if (gtpe.hasNontrivialPrefix) loop(gpre) else None
            val ssym = todo(gsym)
            Some(s.Type(tag = stag, singleType = Some(s.SingleType(spre, ssym))))
          case g.ThisType(gsym) =>
            val stag = t.THIS_TYPE
            val ssym = todo(gsym)
            Some(s.Type(tag = stag, thisType = Some(s.ThisType(ssym))))
          case g.SuperType(gpre, gmix) =>
            val stag = t.SUPER_TYPE
            val spre = loop(gpre)
            val smix = loop(gmix)
            Some(s.Type(tag = stag, superType = Some(s.SuperType(spre, smix))))
          case g.ConstantType(gconst) =>
            def floatBits(x: Float) = java.lang.Float.floatToRawIntBits(x).toLong
            def doubleBits(x: Double) = java.lang.Double.doubleToRawLongBits(x)
            val stag = t.LITERAL_TYPE
            val sconst = gconst match {
              case g.Constant(()) => s.LiteralType(l.UNIT, 0, "")
              case g.Constant(false) => s.LiteralType(l.BOOLEAN, 0, "")
              case g.Constant(true) => s.LiteralType(l.BOOLEAN, 1, "")
              case g.Constant(x: Byte) => s.LiteralType(l.BYTE, x.toLong, "")
              case g.Constant(x: Short) => s.LiteralType(l.SHORT, x.toLong, "")
              case g.Constant(x: Char) => s.LiteralType(l.CHAR, x.toLong, "")
              case g.Constant(x: Int) => s.LiteralType(l.INT, x.toLong, "")
              case g.Constant(x: Long) => s.LiteralType(l.LONG, x, "")
              case g.Constant(x: Float) => s.LiteralType(l.FLOAT, floatBits(x), "")
              case g.Constant(x: Double) => s.LiteralType(l.DOUBLE, doubleBits(x), "")
              case g.Constant(x: String) => s.LiteralType(l.STRING, 0, x)
              case g.Constant(null) => s.LiteralType(l.NULL, 0, "")
              case g.Constant(name: g.TermSymbol) if name.isJavaEnum =>
                s.LiteralType(l.ENUM, 0, name.toSemantic.syntax)
              case gother =>
                sys.error(s"unsupported const ${gother}: ${g.showRaw(gother)}")
            }
            Some(s.Type(tag = stag, literalType = Some(sconst)))
          case g.RefinedType(gparents, gdecls) =>
            val stag = t.COMPOUND_TYPE
            val sparents = gparents.flatMap(loop)
            val sdecls = gdecls.sorted.map(todo)
            Some(s.Type(tag = stag, compoundType = Some(s.CompoundType(sparents, sdecls))))
          case g.AnnotatedType(ganns, gtpe) =>
            val stag = t.ANNOTATED_TYPE
            val stpe = loop(gtpe)
            val sanns = ganns.reverse.flatMap(gann => loop(gann.atp))
            Some(s.Type(tag = stag, annotatedType = Some(s.AnnotatedType(stpe, sanns))))
          case g.ExistentialType(gquants, gtpe) =>
            val stag = t.EXISTENTIAL_TYPE
            val stpe = loop(gtpe)
            val ssyms = gquants.map(todo)
            Some(s.Type(tag = stag, existentialType = Some(s.ExistentialType(stpe, ssyms))))
          case g.ClassInfoType(gparents, gdecls, _) =>
            val stag = t.CLASS_INFO_TYPE
            val sparents = gparents.flatMap(loop)
            val sdecls = gdecls.sorted.map(todo)
            Some(s.Type(tag = stag, classInfoType = Some(s.ClassInfoType(Nil, sparents, sdecls))))
          case g.NullaryMethodType(gtpe) =>
            val stag = t.METHOD_TYPE
            val stpe = loop(gtpe)
            Some(s.Type(tag = stag, methodType = Some(s.MethodType(Nil, Nil, stpe))))
          case gtpe: g.MethodType =>
            def flatten(gtpe: g.Type): (List[List[g.Symbol]], g.Type) = {
              gtpe match {
                case g.MethodType(ghead, gtpe) =>
                  val (gtail, gret) = flatten(gtpe)
                  (ghead :: gtail, gret)
                case gother =>
                  (Nil, gother)
              }
            }
            val (gparamss, gret) = flatten(gtpe)
            val stag = t.METHOD_TYPE
            val sparamss = gparamss.map { gparams =>
              val sparams = gparams.map(todo)
              s.MethodType.ParameterList(sparams)
            }
            val sret = loop(gret)
            Some(s.Type(tag = stag, methodType = Some(s.MethodType(Nil, sparamss, sret))))
          case g.TypeBounds(glo, ghi) =>
            val stag = t.TYPE_TYPE
            val slo = loop(glo)
            val shi = loop(ghi)
            Some(s.Type(tag = stag, typeType = Some(s.TypeType(Nil, slo, shi))))
          case g.PolyType(gtparams, gtpe) =>
            val stparams = gtparams.map(todo)
            val stpe = loop(gtpe)
            stpe.map { stpe =>
              if (stpe.tag == t.CLASS_INFO_TYPE) {
                stpe.update(_.classInfoType.typeParameters := stparams)
              } else if (stpe.tag == t.METHOD_TYPE) {
                stpe.update(_.methodType.typeParameters := stparams)
              } else if (stpe.tag == t.TYPE_TYPE) {
                stpe.update(_.typeType.typeParameters := stparams)
              } else {
                val stag = t.TYPE_LAMBDA
                s.Type(tag = stag, typeLambda = Some(s.TypeLambda(stparams, Some(stpe))))
              }
            }
          case g.NoType =>
            None
          case g.NoPrefix =>
            None
          case g.ErrorType =>
            None
          case gother =>
            sys.error(s"unsupported type ${gother}: ${g.showRaw(gother)}")
        }
      }
      (loop(gtpe), buf.result)
    }
  }

  implicit class XtensionGType(gtpe: g.Type) {
    def hasNontrivialPrefix: Boolean = {
      val (gpre, gsym) = {
        gtpe match {
          case g.TypeRef(gpre, gsym, _) => (gpre, gsym)
          case g.SingleType(gpre, gsym) => (gpre, gsym)
          case _ => return true
        }
      }
      gpre match {
        case g.SingleType(_, gpresym) =>
          gpresym.isTerm && !gpresym.isModule
        case g.ThisType(gpresym) =>
          !gpresym.hasPackageFlag && !gpresym.isModuleOrModuleClass && !gpresym.isConstructor
        case _ =>
          true
      }
    }
  }

  object ByNameType {
    def unapply(gtpe: g.Type): Option[g.Type] = gtpe match {
      case g.TypeRef(_, g.definitions.ByNameParamClass, garg :: Nil) => Some(garg)
      case _ => None
    }
  }

  object RepeatedType {
    def unapply(gtpe: g.Type): Option[g.Type] = gtpe match {
      case g.TypeRef(_, g.definitions.RepeatedParamClass, garg :: Nil) => Some(garg)
      case g.TypeRef(_, g.definitions.JavaRepeatedParamClass, garg :: Nil) => Some(garg)
      case _ => None
    }
  }
}
