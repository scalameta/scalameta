package scala.meta.internal

/** The full document */
final case class Scaladoc(para: Seq[Scaladoc.Paragraph])

/**
 * The available tokens and their documentation are obtained from:
 * @see http://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html
 */
object Scaladoc {

  sealed abstract class Term

  /** A single paragraph of the document */
  final case class Paragraph(term: Seq[Term])

  /* Text */

  /** An inline part of a text block */
  trait TextPart {
    def syntax: String
  }

  /** A description or other inline text block */
  final case class Text(part: Seq[TextPart]) extends Term

  /** A single word, without whitespace */
  final case class Word(value: String) extends TextPart {
    override def syntax: String = value
  }

  /** A reference to a symbol */
  final case class Link(ref: String, anchor: Seq[String], punct: String) extends TextPart {
    override def syntax: String = anchor.mkString(s"[[$ref", " ", s"]]$punct")
  }

  /** A single embedded code expression */
  final case class CodeExpr(code: String, punct: String) extends TextPart {
    override def syntax: String = s"{{{$code}}}$punct"
  }

  /** A block of one or more lines of code */
  final case class CodeBlock(code: Seq[String]) extends Term

  /** A heading */
  final case class Heading(level: Int, title: String) extends Term

  /**
   * A table
   * [[https://www.scala-lang.org/blog/2018/10/04/scaladoc-tables.html]]
   */
  final case class Table(
      header: Table.Row,
      align: Seq[Table.Align],
      row: Seq[Table.Row]
  ) extends Term

  object Table {

    final case class Row(col: Seq[String])

    sealed abstract class Align {
      def leftPad(pad: Int): Int

      /** formats a cell of len `len + 2` (for padding on either side) */
      def syntax(len: Int): String
    }

    private def hyphens(len: Int): String = "-" * len

    final case object Left extends Align {
      override def leftPad(pad: Int): Int = 0
      override def syntax(len: Int): String = ":" + hyphens(1 + len)
    }

    final case object Right extends Align {
      override def leftPad(pad: Int): Int = pad
      override def syntax(len: Int): String = hyphens(1 + len) + ":"
    }

    final case object Center extends Align {
      override def leftPad(pad: Int): Int = pad / 2
      override def syntax(len: Int): String = ":" + hyphens(len) + ":"
    }
  }

  /* List blocks */

  /** Represents a list item */
  final case class ListItem(text: Text, nested: Option[ListBlock] = None)

  /** Represents a list block */
  final case class ListBlock(prefix: String, item: Seq[ListItem]) extends Term

  /* Tags */

  // https://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html#tags
  sealed abstract class TagType(
      val tag: String,
      val hasLabel: Boolean = false,
      val hasDesc: Boolean = false
  )

  /**
   * Represents a tagged documentation remark
   * @param label set iff `tag.hasLabel`
   * @param desc set iff `tag.hasDesc`
   */
  final case class Tag(tag: TagType, label: Word = null, desc: Text = null) extends Term

  object TagType {

    type Base = TagType

    /** Represents an unknown tag */
    final case class UnknownTag(override val tag: String) extends Base(tag, hasDesc = true)

    /* Class specific tags */

    /** Placed in the class comment will describe the primary constructor */
    case object Ctor extends Base("@constructor", hasDesc = true)

    /* Method specific tags */

    /** Detail the return value from a method */
    case object Return extends Base("@return", hasDesc = true)

    /* Method, Constructor and/or Class tags */

    /** What exceptions (if any) the method or constructor may throw */
    case object Throws extends Base("@throws", hasDesc = true)

    /** Detail a value parameter for a method or constructor */
    case object Param extends Base("@param", hasLabel = true, hasDesc = true)

    /** Detail a type parameter for a method, constructor or class */
    case object TypeParam extends Base("@tparam", hasLabel = true, hasDesc = true)

    /* Usage tags */

    /**
     * Reference other sources of information like external document links or
     * related entities in the documentation
     */
    case object See extends Base("@see", hasDesc = true)

    /** Add a note for pre or post conditions, or any other notable restrictions or expectations */
    case object Note extends Base("@note", hasDesc = true)

    /** Provide example code and related descriptions. */
    case object Example extends Base("@example", hasDesc = true)

    /**
     * Provide a simplified method definition for when the full method definition is too complex
     * or noisy
     */
    case object UseCase extends Base("@usecase", hasDesc = true)

    /* Member grouping tags */

    /** Mark the entity as member of a group */
    case object Group extends Base("@group", hasLabel = true)

    /** Provide an optional name for the group */
    case object GroupName extends Base("@groupname", hasLabel = true, hasDesc = true)

    /** Add optional descriptive text to display under the group name */
    case object GroupDesc extends Base("@groupdesc", hasLabel = true, hasDesc = true)

    /** Control the order of the group on the page */
    case object GroupPriority extends Base("@groupprio", hasLabel = true, hasDesc = true)

    /* Diagram tags */

    // @contentDiagram
    // @inheritanceDiagram

    /* Other tags */

    /** Provide author information for the following entity */
    case object Author extends Base("@author", hasDesc = true)

    /** The version of the system or API that this entity is a part of */
    case object Version extends Base("@version", hasDesc = true)

    /** The version of the system or API that this entity was first defined in */
    case object Since extends Base("@since", hasDesc = true)

    /** Documents unimplemented features in an entity */
    case object Todo extends Base("@todo", hasDesc = true)

    /** Marks an entity as deprecated, describing replacement implementation */
    case object Deprecated extends Base("@deprecated", hasDesc = true)

    /**
     * Like [[Deprecated]] but provides advanced warning of
     * planned changes ahead of deprecation.
     */
    case object Migration extends Base("@migration", hasDesc = true)

    /** Take comments from a superclass as defaults if comments are not provided locally */
    case object InheritDoc extends Base("@inheritdoc")

    /** Expand a type alias and abstract type into a full template page */
    case object Documentable extends Base("@documentable", hasDesc = true)

    /* Macros tags */
    // @define <name> <definition>

    /* 2.12 tags */
    // @shortDescription
    // @hideImplicitConversion

    /** Contains all unknown tags */
    val predefined: Seq[Base] = Seq(
      Ctor,
      Return,
      Throws,
      Param,
      TypeParam,
      See,
      Note,
      Example,
      UseCase,
      Group,
      GroupName,
      GroupDesc,
      GroupPriority,
      Author,
      Version,
      Since,
      Todo,
      Deprecated,
      Migration,
      InheritDoc,
      Documentable
    )

  }

}
