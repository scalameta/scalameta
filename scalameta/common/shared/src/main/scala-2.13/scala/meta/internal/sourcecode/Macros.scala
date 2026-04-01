package scala.meta.internal.sourcecode
import scala.language.implicitConversions
import scala.language.existentials


import java.util.concurrent.ConcurrentHashMap
import language.experimental.macros

trait NameMacros {
  implicit def generate: Name = macro Macros.nameImpl
}

trait NameMachineMacros {
  implicit def generate: Name.Machine = macro Macros.nameMachineImpl
}

trait FullNameMacros {
  implicit def generate: FullName = macro Macros.fullNameImpl
}

trait FullNameMachineMacros {
  implicit def generate: FullName.Machine = macro Macros.fullNameMachineImpl
}

trait FileMacros {
  implicit def generate: scala.meta.internal.sourcecode.File = macro Macros.fileImpl
}

trait FileNameMacros {
  implicit def generate: scala.meta.internal.sourcecode.FileName = macro Macros.fileNameImpl
}

trait LineMacros {
  implicit def generate: scala.meta.internal.sourcecode.Line = macro Macros.lineImpl
}

trait EnclosingMacros {
  implicit def generate: Enclosing = macro Macros.enclosingImpl
}

trait EnclosingMachineMacros {
  implicit def generate: Enclosing.Machine = macro Macros.enclosingMachineImpl
}

trait PkgMacros {
  implicit def generate: Pkg = macro Macros.pkgImpl
}

trait TextMacros {
  implicit def generate[T](v: T): Text[T] = macro Macros.text[T]
  def apply[T](v: T): Text[T] = macro Macros.text[T]
}

trait ArgsMacros {
  implicit def generate: Args = macro Macros.argsImpl
}

object Util{
  def isSynthetic(c: Compat.Context)(s: c.Symbol) = isSyntheticName(getName(c)(s))
  def isSyntheticName(name: String) = {
    name == "<init>" || (name.startsWith("<local ") && name.endsWith(">")) || name == "$anonfun"
  }
  def getName(c: Compat.Context)(s: c.Symbol) = s.name.decoded.toString.trim
}

object Macros {

  def nameImpl(c: Compat.Context): c.Expr[Name] = {
    import c.universe._
    var owner = Compat.enclosingOwner(c)
    while(Util.isSynthetic(c)(owner)) owner = owner.owner
    val simpleName = Util.getName(c)(owner)
    c.Expr[scala.meta.internal.sourcecode.Name](q"""${c.prefix}($simpleName)""")
  }

  def nameMachineImpl(c: Compat.Context): c.Expr[Name.Machine] = {
    import c.universe._
    val owner = Compat.enclosingOwner(c)
    val simpleName = Util.getName(c)(owner)
    c.Expr[Name.Machine](q"""${c.prefix}($simpleName)""")
  }

  def fullNameImpl(c: Compat.Context): c.Expr[FullName] = {
    import c.universe._
    val owner = Compat.enclosingOwner(c)
    val fullName =
      owner.fullName.trim
        .split("\\.", -1)
        .filterNot(Util.isSyntheticName)
        .mkString(".")
    c.Expr[scala.meta.internal.sourcecode.FullName](q"""${c.prefix}($fullName)""")
  }

  def fullNameMachineImpl(c: Compat.Context): c.Expr[FullName.Machine] = {
    import c.universe._
    val owner = Compat.enclosingOwner(c)
    val fullName = owner.fullName.trim
    c.Expr[FullName.Machine](q"""${c.prefix}($fullName)""")
  }

  private val filePrefix = "//SOURCECODE_ORIGINAL_FILE_PATH="
  private val filePrefixCache =
    new ConcurrentHashMap[scala.reflect.internal.util.SourceFile, Option[String]]()
  private def findOriginalFile(chars: Array[Char]): Option[String] = {
    new String(chars).linesIterator.find(_.contains(filePrefix)).map(_.split(filePrefix).last)
  }

  def fileImpl(c: Compat.Context): c.Expr[scala.meta.internal.sourcecode.File] = {
    import c.universe._

    val file = filePrefixCache
      .computeIfAbsent(c.enclosingPosition.source, source => findOriginalFile(source.content))
      .getOrElse(c.enclosingPosition.source.path)

    c.Expr[scala.meta.internal.sourcecode.File](q"""${c.prefix}($file)""")
  }

  def fileNameImpl(c: Compat.Context): c.Expr[scala.meta.internal.sourcecode.FileName] = {
    import c.universe._
    val fileName = filePrefixCache
      .computeIfAbsent(c.enclosingPosition.source, source => findOriginalFile(source.content))
      .getOrElse(c.enclosingPosition.source.path)
      .split('/').last
    c.Expr[scala.meta.internal.sourcecode.FileName](q"""${c.prefix}($fileName)""")
  }

  private val linePrefix = "//SOURCECODE_ORIGINAL_CODE_START_MARKER"
  private val linePrefixCache =
    new ConcurrentHashMap[scala.reflect.internal.util.SourceFile, Int]()
  
  def lineImpl(c: Compat.Context): c.Expr[scala.meta.internal.sourcecode.Line] = {
    import c.universe._
    val offset = linePrefixCache.computeIfAbsent(
      c.enclosingPosition.source,
      source => new String(source.content)
        .linesIterator
        .indexWhere(_.contains(linePrefix)) match{
        case -1 => 0
        case n => n + 1
      }
    )

    val line = c.enclosingPosition.line - offset
    c.Expr[scala.meta.internal.sourcecode.Line](q"""${c.prefix}($line)""")
  }

  def enclosingImpl(c: Compat.Context): c.Expr[Enclosing] = enclosing[Enclosing](c)(
    !Util.isSynthetic(c)(_)
  )

  def enclosingMachineImpl(c: Compat.Context): c.Expr[Enclosing.Machine] =
    enclosing[Enclosing.Machine](c)(_ => true)

  def pkgImpl(c: Compat.Context): c.Expr[Pkg] = enclosing[Pkg](c)(_.isPackage)

  def argsImpl(c: Compat.Context): c.Expr[Args] = {
    import c.universe._
    val param = Compat.enclosingParamList(c)
    val texts = param.map(_.map(p => c.Expr[Text[_]](q"""scala.meta.internal.sourcecode.Text($p, ${p.name.toString})""")))
    val textSeqs = texts.map(s => c.Expr(q"""Seq(..$s)"""))
    c.Expr[Args](q"""Seq(..$textSeqs)""")
  }


  def text[T: c.WeakTypeTag](c: Compat.Context)(v: c.Expr[T]): c.Expr[scala.meta.internal.sourcecode.Text[T]] = {
    import c.universe._
    val fileContent = new String(v.tree.pos.source.content)
    val start = v.tree.collect {
      case treeVal => treeVal.pos match {
        case NoPosition ⇒ Int.MaxValue
        case p ⇒ p.startOrPoint
      }
    }.min
    val g = c.asInstanceOf[reflect.macros.runtime.Context].global
    val parser = g.newUnitParser(fileContent.drop(start))
    parser.expr()
    val end = parser.in.lastOffset
    val txt = fileContent.slice(start, start + end)
    val tree = q"""${c.prefix}(${v.tree}, $txt)"""
    c.Expr[scala.meta.internal.sourcecode.Text[T]](tree)
  }
  sealed trait Chunk
  object Chunk{
    case class Pkg(name: String) extends Chunk
    case class Obj(name: String) extends Chunk
    case class Cls(name: String) extends Chunk
    case class Trt(name: String) extends Chunk
    case class Val(name: String) extends Chunk
    case class Var(name: String) extends Chunk
    case class Lzy(name: String) extends Chunk
    case class Def(name: String) extends Chunk

  }

  def enclosing[T](c: Compat.Context)(filter: c.Symbol => Boolean): c.Expr[T] = {

    import c.universe._
    var current = Compat.enclosingOwner(c)
    var path = List.empty[Chunk]
    while(current != NoSymbol && current.toString != "package <root>"){
      if (filter(current)) {

        val chunk = current match {
          case x if x.isPackage => Chunk.Pkg
          case x if x.isModuleClass => Chunk.Obj
          case x if x.isClass && x.asClass.isTrait => Chunk.Trt
          case x if x.isClass => Chunk.Cls
          case x if x.isMethod => Chunk.Def
          case x if x.isTerm && x.asTerm.isVar => Chunk.Var
          case x if x.isTerm && x.asTerm.isLazy => Chunk.Lzy
          case x if x.isTerm && x.asTerm.isVal => Chunk.Val
        }

        path = chunk(Util.getName(c)(current)) :: path
      }
      current = current.owner
    }
    val renderedPath = path.map{
      case Chunk.Pkg(s) => s + "."
      case Chunk.Obj(s) => s + "."
      case Chunk.Cls(s) => s + "#"
      case Chunk.Trt(s) => s + "#"
      case Chunk.Val(s) => s + " "
      case Chunk.Var(s) => s + " "
      case Chunk.Lzy(s) => s + " "
      case Chunk.Def(s) => s + " "
    }.mkString.dropRight(1)
    c.Expr[T](q"""${c.prefix}($renderedPath)""")
  }
}
