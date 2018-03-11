package scala.meta.internal.semanticdb.scalac

import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Scala._
import scala.meta.internal.semanticdb3.SingletonType.{Tag => st}
import scala.meta.internal.semanticdb3.Type.{Tag => t}
import scala.reflect.internal.{Flags => gf}

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
            val stag = t.SINGLETON_TYPE
            val stpe = {
              val stag = st.SYMBOL
              val spre = if (gtpe.hasNontrivialPrefix) loop(gpre) else None
              val ssym = todo(gsym)
              s.SingletonType(stag, spre, ssym, 0, "")
            }
            Some(s.Type(tag = stag, singletonType = Some(stpe)))
          case g.ThisType(gsym) =>
            val stag = t.SINGLETON_TYPE
            val stpe = {
              val stag = st.THIS
              val ssym = todo(gsym)
              s.SingletonType(stag, None, ssym, 0, "")
            }
            Some(s.Type(tag = stag, singletonType = Some(stpe)))
          case g.SuperType(gpre, gmix) =>
            val stag = t.SINGLETON_TYPE
            val stpe = {
              val stag = st.SUPER
              val spre = loop(gpre.typeSymbol.tpe)
              val ssym = todo(gmix.typeSymbol)
              s.SingletonType(stag, spre, ssym, 0, "")
            }
            Some(s.Type(tag = stag, singletonType = Some(stpe)))
          case g.ConstantType(g.Constant(sym: g.TermSymbol)) if sym.hasFlag(gf.JAVA_ENUM) =>
            loop(g.SingleType(sym.owner.thisPrefix, sym))
          case g.ConstantType(g.Constant(_: g.Type)) =>
            loop(gtpe.widen)
          case g.ConstantType(gconst) =>
            val stag = t.SINGLETON_TYPE
            val stpe = {
              def floatBits(x: Float) = java.lang.Float.floatToRawIntBits(x).toLong
              def doubleBits(x: Double) = java.lang.Double.doubleToRawLongBits(x)
              gconst.value match {
                case () => s.SingletonType(st.UNIT, None, "", 0, "")
                case false => s.SingletonType(st.BOOLEAN, None, "", 0, "")
                case true => s.SingletonType(st.BOOLEAN, None, "", 1, "")
                case x: Byte => s.SingletonType(st.BYTE, None, "", x.toLong, "")
                case x: Short => s.SingletonType(st.SHORT, None, "", x.toLong, "")
                case x: Char => s.SingletonType(st.CHAR, None, "", x.toLong, "")
                case x: Int => s.SingletonType(st.INT, None, "", x.toLong, "")
                case x: Long => s.SingletonType(st.LONG, None, "", x, "")
                case x: Float => s.SingletonType(st.FLOAT, None, "", floatBits(x), "")
                case x: Double => s.SingletonType(st.DOUBLE, None, "", doubleBits(x), "")
                case x: String => s.SingletonType(st.STRING, None, "", 0, x)
                case null => s.SingletonType(st.NULL, None, "", 0, "")
                case _ => sys.error(s"unsupported const ${gconst}: ${g.showRaw(gconst)}")
              }
            }
            Some(s.Type(tag = stag, singletonType = Some(stpe)))
          case g.RefinedType(gparents, gdecls) =>
            val stag = t.STRUCTURAL_TYPE
            val stpe = {
              val sparents = gparents.flatMap(loop)
              Some(s.Type(tag = t.WITH_TYPE, withType = Some(s.WithType(sparents))))
            }
            val sdecls = gdecls.sorted.map(todo)
            Some(s.Type(tag = stag, structuralType = Some(s.StructuralType(stpe, sdecls))))
          case g.AnnotatedType(ganns, gtpe) =>
            val stag = t.ANNOTATED_TYPE
            val sanns = {
              ganns.reverse.map { gann =>
                val (sann, todo) = gann.toSemantic
                todo.foreach(buf.+=)
                sann
              }
            }
            val stpe = loop(gtpe)
            Some(s.Type(tag = stag, annotatedType = Some(s.AnnotatedType(sanns, stpe))))
          case g.ExistentialType(gtparams, gtpe) =>
            val stag = t.EXISTENTIAL_TYPE
            val stparams = gtparams.map(todo)
            val stpe = loop(gtpe)
            Some(s.Type(tag = stag, existentialType = Some(s.ExistentialType(stparams, stpe))))
          case g.ClassInfoType(gparents, gdecls, _) =>
            val stag = t.CLASS_INFO_TYPE
            val sparents = gparents.flatMap(loop)
            val sdecls = gdecls.sorted.map(todo) ++ gtpe.javaCompanionDecls.map(todo)
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
                val stag = t.UNIVERSAL_TYPE
                s.Type(tag = stag, universalType = Some(s.UniversalType(stparams, Some(stpe))))
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
    def javaCompanionDecls: List[g.Symbol] = {
      if (gtpe.typeSymbol.isJavaClass) {
        gtpe.typeSymbol.companionModule.info match {
          case m: g.ModuleTypeRef =>
            val decls = m.sym.info.decls.sorted
            // static java classes don't have <init> constructors.
            decls.dropWhile(_.name == g.nme.CONSTRUCTOR)
          case _ => Nil
        }
      } else {
        Nil
      }
    }
    // TODO: Implement me.
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
    def descriptor: String = {
      def paramDescriptors = gtpe.paramss.flatten.map(_.info.descriptor)
      gtpe match {
        case ByNameType(gtpe) => "=>" + gtpe.descriptor
        case RepeatedType(gtpe) => gtpe.descriptor + "*"
        case g.TypeRef(_, gsym, _) => gsym.name.toSemantic.encoded
        case g.SingleType(_, _) => ".type"
        case g.ThisType(_) => ".type"
        case g.ConstantType(g.Constant(_: g.Type)) => "Class"
        case g.ConstantType(_) => ".type"
        case g.RefinedType(_, _) => "{}"
        case g.AnnotatedType(_, gtpe) => gtpe.descriptor
        case g.ExistentialType(_, gtpe) => gtpe.descriptor
        case _: g.NullaryMethodType | _: g.MethodType => paramDescriptors.mkString(",")
        case g.PolyType(_, gtpe) => gtpe.descriptor
        case other => "?"
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
