package scala.meta.internal.semanticdb.scalac

import org.scalameta.collections._
import scala.reflect.ClassTag
import scala.reflect.macros.Attachments
import scala.tools.nsc.Global
import scala.tools.nsc.doc.ScaladocGlobal
import scala.tools.nsc.interactive.{Global => InteractiveGlobal}
import scala.tools.nsc.interpreter.ReplGlobal

trait ReflectionToolkit {
  val global: Global
  import global._
  lazy val g: global.type = global

  lazy val isDocCompiler = global.isInstanceOf[ScaladocGlobal]
  lazy val isReplCompiler = global.isInstanceOf[ReplGlobal]
  lazy val isInteractiveCompiler = global.isInstanceOf[InteractiveGlobal]
  // NOTE: InteractiveGlobal does not work with semanticdb-scalac, in scalameta/language-server
  // we tried to enable semanticdb-scalac with the presentation compiler and it resulted
  // in cryptic infinite while loops while completing scope members.
  lazy val isSupportedCompiler = !isDocCompiler && !isReplCompiler && !isInteractiveCompiler

  // NOTE: this boilerplate is unfortunately necessary, because we don't expose Attachable in the public API
  trait Attachable[-T] {
    def attachments(carrier: T): Attachments { type Pos = Position }
    def updateAttachment[U: ClassTag](carrier: T, attachment: U): Unit
  }
  object Attachable {
    implicit object TreeAttachable extends Attachable[Tree] {
      def attachments(carrier: Tree): Attachments { type Pos = Position } = carrier.attachments
      def updateAttachment[U: ClassTag](carrier: Tree, attachment: U): Unit =
        carrier.updateAttachment(attachment)
    }
    implicit object SymbolAttachable extends Attachable[Symbol] {
      def attachments(carrier: Symbol): Attachments { type Pos = Position } = carrier.attachments
      def updateAttachment[U: ClassTag](carrier: Symbol, attachment: U): Unit =
        carrier.updateAttachment(attachment)
    }
  }
  implicit class XtensionAttachable[T: Attachable](carrier: T) {
    def attachments: Attachments { type Pos = Position } =
      implicitly[Attachable[T]].attachments(carrier)
    def updateAttachment[U: ClassTag](attachment: U): Unit =
      implicitly[Attachable[T]].updateAttachment(carrier, attachment)
  }

  // NOTE: the mechanism of attachments is too low-level for our purposes
  // because it typically works with dedicated data structures that are declared and then shared between all potential users
  // since we are a compiler plugin, that's a big restriction, so we go for a stringly-typed approach
  // all metadata produced by the plugin is stored as key-value pairs in a java.util.HashMap[String, Any]
  // why Java's HashMap and not a native Scala Map? because prior to 2.11.2 attachments don't work  with subtyping and Map has a bunch of specific subclasses
  implicit class XtensionMetadataAttachable[T: Attachable](carrier: T) {
    def metadata: Metadata[T] = new Metadata(carrier)
    def appendMetadata(kvps: (String, Any)*): T = {
      kvps.foreach(kvp => carrier.metadata += kvp); carrier
    }
    def removeMetadata(keys: String*): T = {
      keys.foreach(key => carrier.metadata -= key); carrier
    }
    def hasMetadata(key: String): Boolean = metadata.get(key).isDefined
  }

  class Metadata[T: Attachable](carrier: T) {
    def toMap: Map[String, Any] =
      carrier.attachments
        .get[java.util.HashMap[String, Any]]
        .map(_.toScalaMap)
        .getOrElse(Map[String, Any]())
    def toOption: Option[Map[String, Any]] =
      carrier.attachments.get[java.util.HashMap[String, Any]].map(_.toScalaMap)
    def transform(f: Map[String, Any] => Map[String, Any]): Unit =
      carrier.updateAttachment(f(toMap).toJavaMap)
    def contains(key: String): Boolean = toMap.contains(key)
    def apply(key: String): Any = toMap(key)
    def get(key: String): Option[Any] = toMap.get(key)
    def getOrElse[T: ClassTag](key: String, value: => T): T =
      toMap.get(key).map(_.asInstanceOf[T]).getOrElse(value)
    def getOrElseUpdate[T: ClassTag](key: String, default: => T): T = {
      val valueopt = toMap.get(key).map(_.asInstanceOf[T])
      val value = valueopt.getOrElse(default)
      if (valueopt.isEmpty) update(key, value)
      value
    }
    def update(key: String, value: Any): Unit = transform(_ + (key -> value))
    def remove(key: String): Unit = transform(_ - key)
    def +=(kvp: (String, Any)): Unit = update(kvp._1, kvp._2)
    def ++=(other: Map[String, Any]): Unit = transform(_ ++ other)
    def ++=(other: Metadata[T]): Unit = transform(_ ++ other.toMap)
    def -=(key: String): Unit = remove(key)
    def --=(other: List[String]): Unit = transform(_ -- other)
    def --=(other: Metadata[T]): Unit = transform(_ -- other.toMap.keys)
    override def toString = toMap.toString
  }

  class CompilationUnitCache(unit: CompilationUnit) {
    def getOrElse[T](key: String, op: => T): T = {
      val dummyName = g.TermName("<" + key + "Carrier>")
      val dummySymbol = unit.checkedFeatures.find(_.name == dummyName)
      val cached = dummySymbol.flatMap(_.metadata.get(key).map(_.asInstanceOf[T]))
      cached.getOrElse({
        val computed = op
        val dummySymbol = g.NoSymbol.newValue(dummyName).appendMetadata(key -> computed)
        unit.checkedFeatures += dummySymbol
        computed
      })
    }
  }

  implicit class XtensionCompilationUnitCache(unit: CompilationUnit) {
    def cache: CompilationUnitCache = new CompilationUnitCache(unit)
  }

  implicit class XtensionDesugarings[T: Attachable](carrier: T) {
    private def rememberOriginal(designation: String, original: Tree): T = {
      if (carrier == original) carrier
      else carrier.appendMetadata(designation -> original)
    }
    def rememberConstfoldOf(original: Tree) = rememberOriginal("constantFoldingOriginal", original)
    def rememberClassOf(original: Tree) = rememberOriginal("classOfOriginal", original)
    def rememberNewArrayOf(original: Tree) = rememberOriginal("newArrayOriginal", original)
    def rememberSingletonTypeTreeOf(original: Tree) =
      rememberOriginal("singletonTypeTreeOriginal", original)
    // def rememberCompoundTypeTreeOf(original: Tree) = rememberOriginal("compoundTypeTreeOriginal", original)
    def rememberExistentialTypeTreeOf(original: Tree) =
      rememberOriginal("existentialTypeTreeOriginal", original)
    def rememberAnnotatedOf(original: Tree) = rememberOriginal("annotatedOriginal", original)
    def rememberSelfTypeOf(original: Tree) = rememberOriginal("selfTypeOriginal", original)
    def rememberSelectOf(original: Tree) = rememberOriginal("selectOriginal", original)
  }

  object ConstfoldOf {
    def unapply[T: Attachable](carrier: T) =
      carrier.metadata.get("constantFoldingOriginal").map(_.asInstanceOf[Tree])
  }

  object ClassOf {
    def unapply[T: Attachable](carrier: T) =
      carrier.metadata.get("classOfOriginal").map(_.asInstanceOf[Tree])
  }

  object NewArrayOf {
    def unapply[T: Attachable](carrier: T) =
      carrier.metadata.get("newArrayOriginal").map(_.asInstanceOf[Tree])
  }

  object SingletonTypeTreeOf {
    def unapply[T: Attachable](carrier: T) =
      carrier.metadata.get("singletonTypeTreeOriginal").map(_.asInstanceOf[Tree])
  }

  object CompoundTypeTreeOf {
    def unapply[T: Attachable](carrier: T) = carrier match {
      case CompoundTypeTree(templ) =>
        val att = templ.attachments.get[CompoundTypeTreeOriginalAttachment]
        templ.removeAttachment[CompoundTypeTreeOriginalAttachment]
        att.map(att =>
          CompoundTypeTree(treeCopy.Template(templ, att.parents, templ.self, att.stats)))
      case _ =>
        None
    }
  }

  object ExistentialTypeTreeOf {
    def unapply[T: Attachable](carrier: T) =
      carrier.metadata.get("existentialTypeTreeOriginal").map(_.asInstanceOf[Tree])
  }

  object AnnotatedOf {
    def unapply[T: Attachable](carrier: T) =
      carrier.metadata.get("annotatedOriginal").map(_.asInstanceOf[Tree])
  }

  object SelfTypeOf {
    def unapply[T: Attachable](carrier: T) =
      carrier.metadata.get("selfTypeOriginal").map(_.asInstanceOf[Tree])
  }

  object SelectOf {
    def unapply[T: Attachable](carrier: T) =
      carrier.metadata.get("selectOriginal").map(_.asInstanceOf[Tree])
  }
}
