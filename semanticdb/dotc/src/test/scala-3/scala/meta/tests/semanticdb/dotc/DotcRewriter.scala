package scala.meta.tests.semanticdb.dotc

import scala.meta._

import java.nio.file.Path

import scala.collection.immutable.SortedMap

/** Edits ScalaPB's output so that it needs neither scalapb-runtime nor protobuf-java. */
object DotcRewriter {

  private val Renames = Map(
    "scalapb.GeneratedEnum" -> "SemanticdbGeneratedEnum",
    "scalapb.GeneratedEnumCompanion" -> "SemanticdbGeneratedEnumCompanion",
    "scalapb.GeneratedMessage" -> "SemanticdbGeneratedMessage",
    "scalapb.GeneratedMessageCompanion" -> "SemanticdbGeneratedMessageCompanion",
    "scalapb.GeneratedOneof" -> "SemanticdbGeneratedOneof",
    "scalapb.GeneratedSealedOneof" -> "SemanticdbGeneratedSealedOneof",
    "scalapb.LiteParser" -> "LiteParser",
    "scalapb.TypeMapper" -> "SemanticdbTypeMapper",
    "scalapb.UnrecognizedEnum" -> "SemanticdbUnrecognizedEnum",
    "com.google.protobuf.CodedInputStream" -> "SemanticdbInputStream",
    "com.google.protobuf.CodedOutputStream" -> "SemanticdbOutputStream",
  )

  /** The members of ScalaPB's reflection and text format, which need its descriptors. */
  private val Removed = Set(
    "enumCompanionForFieldNumber",
    "getField",
    "getFieldByNumber",
    "javaDescriptor",
    "messageCompanionForFieldNumber",
    "messageReads",
    "nestedMessagesCompanions",
    "scalaDescriptor",
    "toProtoString",
  )

  /**
   * The parents of the types that derive `CanEqual`, apart from a case class of a oneof: a message
   * class, a sealed oneof trait, a oneof trait and an enum class.
   */
  private val Derived = Set(
    "scalapb.GeneratedMessage",
    "scalapb.GeneratedSealedOneof",
    "scalapb.GeneratedOneof",
    "scalapb.GeneratedEnum",
  )

  private val FileObject = "scalapb.GeneratedFileObject"

  /** The annotations that Scala 3's copy of the classes has, by the simple name of their type. */
  private val KeptAnnotations = Set("SerialVersionUID", "transient", "sharable")

  private val RuntimeImport = "\nimport dotty.tools.dotc.semanticdb.internal.*"
  private val SharableImport = "\nimport scala.annotation.internal.sharable"

  private final case class Edit(start: Int, end: Int, text: String)

  /**
   * Returns the rewritten files of ScalaPB's output, by path relative to its directory, with `\n`
   * line endings.
   *
   * @param raw
   *   the directory of ScalaPB's output
   */
  def apply(raw: Path): SortedMap[String, String] = DotcOutput.read(raw)
    .flatMap { case (path, text) => rewrite(path, text.replace("\r\n", "\n")).map(path -> _) }

  /**
   * Returns whether the output may have `annot`.
   *
   * @param annot
   *   the annotation, in any spelling of its type
   */
  def kept(annot: Mod.Annot): Boolean = annot.init.tpe match {
    case t: Type.Select => KeptAnnotations(t.name.value)
    case t: Type.Name => KeptAnnotations(t.value)
    case _ => false
  }

  private def ref(tree: Tree): String = tree.pos.text.stripPrefix("_root_.")

  private def parents(templ: Template): List[String] = templ.inits.map(_.tpe match {
    case t: Type.Apply => ref(t.tpe)
    case t => ref(t)
  })

  private def names(pats: List[Pat]): List[String] = pats.collect { case p: Pat.Var =>
    p.name.value
  }

  private def rewrite(path: String, text: String): Option[String] = {
    val source = dialects.Scala3(Input.VirtualFile(path, text)).parse[Source].get
    val fileObjects = source
      .collect { case t: Defn.Object if parents(t.templ).contains(FileObject) => t }
    if (fileObjects.nonEmpty) None
    else {
      val oneofs = source.collect {
        case t: Defn.Trait if parents(t.templ).contains("scalapb.GeneratedOneof") => t.name.value
      }.toSet
      val edits = List.newBuilder[Edit]
      val sharables = List.newBuilder[Edit]
      def derive(templ: Template): Unit = {
        val end = templ.inits.last.pos.end
        edits += Edit(end, end, " derives CanEqual")
      }
      def sharable(mods: List[Mod]): Unit = mods.foreach {
        case m: Mod.Annot if ref(m.init.tpe) == "transient" =>
          sharables += Edit(m.pos.end, m.pos.end, " @sharable")
        case _ =>
      }
      new Traverser {
        override def apply(tree: Tree): Unit = tree match {
          case t: Defn.Def if Removed(t.name.value) => edits += removal(text, t.pos)
          case t: Defn.Val if names(t.pats).exists(Removed) => edits += removal(text, t.pos)
          case t: Term.Annotate => apply(t.expr)
          case t: Mod.Annot if !kept(t) => edits += removal(text, t.pos)
          case t: Defn.Val if names(t.pats).exists(_.startsWith("_typemapper_")) =>
            sharable(t.mods)
            super.apply(t)
          case t: Defn.Var if names(t.pats).contains("__serializedSizeMemoized") =>
            sharable(t.mods)
            super.apply(t)
          case t: Defn.Class =>
            val ps = parents(t.templ)
            if (ps.exists(Derived) ||
              t.mods.exists(_.is[Mod.Case]) && ps.exists(p => oneofs(p.split('.').last)))
              derive(t.templ)
            super.apply(t)
          case t: Defn.Trait if parents(t.templ).exists(Derived) =>
            derive(t.templ)
            super.apply(t)
          case t @ (_: Term.Select | _: Type.Select) => Renames.get(ref(t)) match {
              case Some(name) => edits += Edit(t.pos.start, t.pos.end, name)
              case None => super.apply(t)
            }
          case _ => super.apply(tree)
        }
      }.apply(source)
      val shared = sharables.result()
      val pkg = source.stats.collectFirst { case p: Pkg => p.ref.pos.end }.getOrElse(0)
      val imports = RuntimeImport + (if (shared.isEmpty) "" else SharableImport)
      edits ++= shared
      edits += Edit(pkg, pkg, imports)
      Some(applyEdits(path, text, edits.result().sortBy(e => (e.start, e.end))))
    }
  }

  /**
   * Returns an edit to delete `pos` from `text`, with its lines if nothing else is on them, and
   * else with the spaces between `pos` and the rest of its line.
   */
  private def removal(text: String, pos: Position): Edit = {
    val lineStart = text.lastIndexOf('\n', pos.start - 1) + 1
    val lineEnd = text.indexOf('\n', pos.end) match {
      case -1 => text.length
      case i => i + 1
    }
    def blank(from: Int, to: Int) = text.substring(from, to).trim.isEmpty
    if (!blank(pos.end, lineEnd)) Edit(pos.start, text.indexWhere(_ != ' ', pos.end), "")
    else if (blank(lineStart, pos.start)) Edit(lineStart, lineEnd, "")
    else Edit(text.lastIndexWhere(_ != ' ', pos.start - 1) + 1, pos.end, "")
  }

  private def applyEdits(path: String, text: String, edits: List[Edit]): String = {
    val sb = new java.lang.StringBuilder
    val end = edits.foldLeft(0) { (pos, edit) =>
      if (edit.start < pos)
        throw new IllegalStateException(s"$path: edits overlap at [${edit.start}, $pos)")
      sb.append(text, pos, edit.start).append(edit.text)
      edit.end
    }
    sb.append(text, end, text.length).toString
  }

}
