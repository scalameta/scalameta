package scala.meta.internal

/** The full document */
final case class Scaladoc(para: Seq[Scaladoc.Paragraph])

/**
 * The available tokens and their documentation are obtained from:
 * @see
 *   http://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html
 */
object Scaladoc {

  sealed abstract class Term

  /** A single paragraph of the document */
  final case class Paragraph(terms: Seq[Term])

  /* Text */

  /** An inline part of a text block */
  trait TextPart {
    def syntax: String
  }

  /** A description or other inline text block */
  final case class Text(parts: Seq[TextPart]) extends Term

  /** A single word, without whitespace */
  final case class Word(value: String) extends TextPart {
    override def syntax: String = value
  }

  /** A reference to a symbol */
  final case class Link(ref: String, anchor: Seq[String], punct: String = "") extends TextPart {
    def this(parts: Seq[String], punct: String) = this(parts.head, parts.tail, punct)
    override def syntax: String = {
      val sb = new StringBuilder
      sb.append("[[").append(ref)
      anchor.foreach(x => sb.append(' ').append(x))
      sb.append("]]").append(punct)
      sb.result()
    }
  }

  /** A single embedded code expression */
  final case class CodeExpr(code: String, punct: String = "") extends TextPart {
    override def syntax: String = s"{{{$code}}}$punct"
  }

  /** A markdown code span, an embedded code expression */
  final case class MdCodeSpan(code: String, fence: String, punct: String = "") extends TextPart {
    override def syntax: String = s"$fence$code$fence$punct"
  }

  /** Represents an enclosed tagged documentation remark */
  final case class EnclosedJavaTag(tag: String, desc: Seq[String] = Nil) extends TextPart {
    override def syntax: String = (tag +: desc).mkString("{", " ", "}")
  }

  /** A block of one or more lines of code */
  final case class CodeBlock(code: Seq[String]) extends Term

  /** A markdown block of one or more lines of code */
  final case class MdCodeBlock(info: Seq[String], code: Seq[String], fence: String) extends Term

  /** A heading */
  final case class Heading(level: Int, title: String) extends Term

  /**
   * A table [[https://www.scala-lang.org/blog/2018/10/04/scaladoc-tables.html]]
   */
  final case class Table(header: Table.Row, align: Seq[Table.Align], rows: Seq[Table.Row])
      extends Term

  object Table {

    final case class Row(cols: Seq[String])

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

  sealed abstract class ListType
  object ListType {
    type Base = ListType
    case object Bullet extends Base
    case object Decimal extends Base
    case object Roman extends Base
    case object Alpha extends Base
  }

  /** Represents a list item */
  final case class ListItem(prefix: String, text: Text, terms: Seq[Term] = Nil)

  /** Represents a list block */
  final case class ListBlock(listType: ListType, items: Seq[ListItem]) extends Term

  /* Tags */

  // https://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html#tags
  sealed abstract class TagType(
      val tag: String,
      val hasLabel: Boolean = false,
      val optDesc: Boolean = true
  )

  /**
   * Represents a tagged documentation remark
   * @param label
   *   set iff `tag.hasLabel`
   * @param desc
   *   set iff `tag.hasDesc`
   * @note
   *   if label is expected but desc is not, label will contain the entire tag line
   */
  final case class Tag(tag: TagType, label: Option[Word] = None, desc: Seq[Term] = Nil) extends Term

  object TagType {

    type Base = TagType

    /** Represents an unknown tag */
    final case class UnknownTag(override val tag: String) extends Base(tag)

    /* Class specific tags */

    /** Placed in the class comment will describe the primary constructor */
    case object Ctor extends Base("@constructor")

    /* Method specific tags */

    /** Detail the return value from a method */
    case object Return extends Base("@return")

    /* Method, Constructor and/or Class tags */

    /** What exceptions (if any) the method or constructor may throw */
    case object Throws extends Base("@throws", hasLabel = true)

    /** Detail a value parameter for a method or constructor */
    case object Param extends Base("@param", hasLabel = true)

    /** Detail a type parameter for a method, constructor or class */
    case object TypeParam extends Base("@tparam", hasLabel = true)

    /* Usage tags */

    /**
     * Reference other sources of information like external document links or related entities in
     * the documentation
     */
    case object See extends Base("@see")

    /** Add a note for pre or post conditions, or any other notable restrictions or expectations */
    case object Note extends Base("@note")

    /** Provide example code and related descriptions. */
    case object Example extends Base("@example")

    /**
     * Provide a simplified method definition for when the full method definition is too complex or
     * noisy
     */
    case object UseCase extends Base("@usecase", hasLabel = true, optDesc = false)

    /* Member grouping tags */

    /** Mark the entity as member of a group */
    case object Group extends Base("@group", hasLabel = true, optDesc = false)

    /** Provide an optional name for the group */
    case object GroupName extends Base("@groupname", hasLabel = true, optDesc = false)

    /** Add optional descriptive text to display under the group name */
    case object GroupDesc extends Base("@groupdesc", hasLabel = true)

    /** Control the order of the group on the page */
    case object GroupPriority extends Base("@groupprio", hasLabel = true, optDesc = false)

    /* Diagram tags */

    // @contentDiagram
    // @inheritanceDiagram

    /* Other tags */

    /** Provide author information for the following entity */
    case object Author extends Base("@author")

    /** The version of the system or API that this entity is a part of */
    case object Version extends Base("@version", hasLabel = true, optDesc = false)

    /** The version of the system or API that this entity was first defined in */
    case object Since extends Base("@since", hasLabel = true)

    /** Documents unimplemented features in an entity */
    case object Todo extends Base("@todo")

    /** Marks an entity as deprecated, describing replacement implementation */
    case object Deprecated extends Base("@deprecated")

    /**
     * Like [[Deprecated]] but provides advanced warning of planned changes ahead of deprecation.
     */
    case object Migration extends Base("@migration")

    /** Take comments from a superclass as defaults if comments are not provided locally */
    case object InheritDoc extends Base("@inheritdoc", optDesc = false)

    /** Expand a type alias and abstract type into a full template page */
    case object Documentable extends Base("@documentable")

    /* Macros tags */

    /**
     * Allows use of {{{label}}} in other Scaladoc comments within the same source file which will
     * be expanded to the contents of {{{definition}}}.
     */
    case object Define extends Base("@define", hasLabel = true)

    /* 2.12 tags */
    // @shortDescription
    // @hideImplicitConversion

    /** Contains all known tags */
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
      Define,
      Author,
      Version,
      Since,
      Todo,
      Deprecated,
      Migration,
      InheritDoc,
      Documentable
    )

    val tagTypeMap = predefined.map(x => x.tag -> x).toMap

    def getTag(tag: String): TagType = tagTypeMap.getOrElse(tag, TagType.UnknownTag(tag))
  }

}
