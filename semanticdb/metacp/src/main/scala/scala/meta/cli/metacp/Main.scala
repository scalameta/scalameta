package scala.meta.cli.metacp

import java.io._
import java.nio.file._
import scala.collection.mutable
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.reflect.NameTransformer
import scala.tools.scalap.scalax.rules.scalasig._
import scala.util.control.NonFatal
import org.langmeta.io._

object Main {
  def main(args: Array[String]): Unit = {
    Settings.parse(args.toList) match {
      case Some(settings) => sys.exit(process(settings))
      case None => sys.exit(1)
    }
  }

  def process(settings: Settings): Int = {
    var failed = false
    val classpath = Classpath(settings.cps.mkString(File.pathSeparator))
    val fragments = classpath.deep.filter(_.uri.toString.endsWith(".class"))
    fragments.sortBy(_.uri.toString).foreach { fragment =>
      try {
        val bytecode = {
          val is = fragment.uri.toURL.openStream()
          val os = new ByteArrayOutputStream()
          var b = is.read()
          while (b != -1) {
            os.write(b)
            b = is.read()
          }
          is.close()
          ByteCode(os.toByteArray())
        }
        val classfile = ClassFileParser.parse(bytecode)
        ScalaSigParser.parse(classfile) match {
          case Some(scalaSig) =>
            val semanticdbInfos = scalaSig.symbols.flatMap {
              case sym: SymbolInfoSymbol => sinfo(sym)
              case _ => None
            }
            val semanticdbRoot = Paths.get(settings.d, "META-INF", "semanticdb")
            val className = NameTransformer.decode(fragment.name.toString)
            val semanticdbName = className.replace(".class", ".semanticdb")
            val semanticdbFile = semanticdbRoot.resolve(semanticdbName)
            semanticdbFile.getParent.toFile.mkdirs()
            val semanticdbDocument = s.TextDocument(
              schema = s.Schema.SEMANTICDB3,
              uri = className.replace(".class", ".scala"),
              language = "Scala",
              symbols = semanticdbInfos)
            val semanticdbDocuments = s.TextDocuments(List(semanticdbDocument))
            val semanticdbStream = Files.newOutputStream(semanticdbFile)
            semanticdbDocuments.writeTo(semanticdbStream)
            semanticdbStream.close()
          case None =>
            ()
        }
      } catch {
        case NonFatal(ex) =>
          println(s"error: can't convert $fragment")
          ex.printStackTrace()
          failed = true
      }
    }
    if (failed) 1 else 0
  }

  private def sinfo(sym: SymbolInfoSymbol): Option[s.SymbolInformation] = {
    if (sym.isSynthetic) return None
    if (sym.isModuleClass) return None
    if (sym.name.endsWith(" ")) return None
    if (sym.name.endsWith("_$eq")) return None
    if (sym.name == "<init>" && !sym.parent.map(_.isClass).getOrElse(false)) return None
    Some(s.SymbolInformation(
      symbol = ssymbol(sym),
      kind = skind(sym),
      properties = sproperties(sym),
      name = sname(sym),
      tpe = Some(stpe(sym))))
  }

  // NOTE: ExternalSymbol has empty flags, so we can't really convert it.
  // Maybe there's some way to get from ExternalSymbol to SymbolInfoSymbol,
  // but it's not obvious from the scalap API.
  private def ssymbol(sym: SymbolInfoSymbol): String = {
    val prefix = {
      sym.parent match {
        case Some(parent: SymbolInfoSymbol) => ssymbol(parent)
        case Some(parent: ExternalSymbol) => parent.path + "."
        case _ => sys.error(s"unsupported symbol $sym")
      }
    }
    val encodedName = {
      val name = sname(sym)
      if (name.isEmpty) sys.error(s"unsupported symbol $sym")
      val (start, parts) = (name.head, name.tail)
      val isStartOk = Character.isJavaIdentifierStart(start)
      val isPartsOk = parts.forall(Character.isJavaIdentifierPart)
      if (isStartOk && isPartsOk) name
      else "`" + name + "`"
    }
    skind(sym) match {
      case k.VAL | k.VAR | k.OBJECT | k.PACKAGE | k.PACKAGE_OBJECT =>
        prefix + encodedName + "."
      case k.DEF | k.PRIMARY_CONSTRUCTOR | k.SECONDARY_CONSTRUCTOR | k.MACRO =>
        val descriptor = {
          // TODO: We're pretty much fatally blocked here.
          // Looks like we'll have to change the specification of SemanticDB.
          def loop(tpe: Type): String = {
            tpe match {
              case MethodType(_, params) => "(" + params.length + ")"
              case NullaryMethodType(_) => "(0)"
              case PolyType(tpe, _) => loop(tpe)
              case _ => "<?>"
            }
          }
          loop(sym.infoType)
        }
        prefix + encodedName + descriptor + "."
      case k.TYPE | k.CLASS | k.TRAIT =>
        prefix + encodedName + "#"
      case k.PARAMETER =>
        prefix + "(" + encodedName + ")"
      case k.TYPE_PARAMETER =>
        prefix + "[" + encodedName + "]"
      case skind =>
        sys.error(s"unsupported kind $skind for symbol $sym")
    }
  }

  private val primaryCtors = mutable.Map[String, Int]()
  private def skind(sym: Symbol): s.SymbolInformation.Kind = {
    sym match {
      case sym: MethodSymbol if sym.isAccessor && !sym.isParamAccessor =>
        // TODO: Implement k.VAR.
        // Currently, we filter out methods that end in `_=`,
        // which means that we never get symbols that are mutable.
        if (sym.isMutable) k.VAR
        else k.VAL
      case sym: MethodSymbol if sym.isParamAccessor || sym.isParam =>
        // NOTE: This is some craziness - parameters are modelled as methods.
        // Not just class parameters, but also normal method parameters.
        k.PARAMETER
      case sym: MethodSymbol if sym.name == "<init>" =>
        val primaryIndex = primaryCtors.getOrElseUpdate(sym.path, sym.entry.index)
        if (sym.entry.index == primaryIndex) k.PRIMARY_CONSTRUCTOR
        else k.SECONDARY_CONSTRUCTOR
      case sym: MethodSymbol =>
        // NOTE: More craziness - 0x8000 used to mean DEPRECATED back then.
        // Since then, deprecated became an annotation, and 0x8000 got used by MACRO,
        // but Scalap hasn't been updated.
        if (sym.isDeprecated) k.MACRO
        else k.DEF
      case sym: TypeSymbol if sym.isParam =>
        k.TYPE_PARAMETER
      case _: TypeSymbol | _: AliasSymbol =>
        k.TYPE
      case sym: ClassSymbol if !sym.isModule =>
        if (sym.isTrait) k.TRAIT
        else k.CLASS
      case _: ObjectSymbol | _: ClassSymbol =>
        if (sym.name == "package") k.PACKAGE_OBJECT
        else k.OBJECT
      case _ =>
        sys.error(s"unsupported symbol $sym")
    }
  }

  private def sproperties(sym: Symbol): Int = {
    def isAbstractClass = sym.isAbstract && !sym.isTrait
    def isAbstractMethod = sym.isMethod && sym.isDeferred
    def isAbstractType = sym.isType && !sym.isParam && sym.isDeferred
    var sproperties = 0
    def sflip(sbit: Int) = sproperties ^= sbit
    if (!sym.isParamAccessor && (sym.isPrivate || sym.isLocal)) sflip(p.PRIVATE.value)
    if (sym.isProtected) sflip(p.PROTECTED.value)
    if (isAbstractClass || isAbstractMethod || isAbstractType) sflip(p.ABSTRACT.value)
    if (sym.isFinal || sym.isModule) sflip(p.FINAL.value)
    if (sym.isSealed) sflip(p.SEALED.value)
    if (sym.isImplicit) sflip(p.IMPLICIT.value)
    if (sym.isLazy) sflip(p.LAZY.value)
    if (sym.isCase) sflip(p.CASE.value)
    if (sym.isType && sym.isCovariant) sflip(p.COVARIANT.value)
    if (sym.isType && sym.isContravariant) sflip(p.CONTRAVARIANT.value)
    if (sym.isAccessor && sym.isParamAccessor && !sym.isMutable) sflip(p.VALPARAM.value)
    if (sym.isAccessor && sym.isParamAccessor && sym.isMutable) sflip(p.VARPARAM.value)
    sproperties
  }

  private def sname(sym: Symbol): String = {
    if (sym.name == "<init>") "<init>"
    else NameTransformer.decode(sym.name)
  }

  private def stpe(sym: SymbolInfoSymbol): s.Type = {
    s.Type(tag = s.Type.Tag.UNKNOWN_TAG)
  }

  private implicit class SymbolOps(sym: Symbol) {
    def isModuleClass = sym.isInstanceOf[ClassSymbol] && sym.isModule
    def isClass = sym.isInstanceOf[ClassSymbol] && !sym.isTrait && !sym.isModule
    def isType = sym.isInstanceOf[TypeSymbol]
  }
}
