package scala.meta.contrib

import fastparse.all._
import fastparse.core.Parsed

/**
  * Represents a scaladoc line.
  */
case class DocToken(kind: DocToken.Kind, name: Option[String], body: Option[String]) {

  override def toString: String = {
    ((name, body) match {
      case (Some(n), _) => s"$kind(name=$n, body=${body.getOrElse("")})"
      case (None, Some(kindBody)) => s"$kind($kindBody)"
      case _ => kind.toString
    }).replaceAll("\n", " ")
  }

  /**
    * Obtains all the code references from this DocToken.
    */
  def references: List[DocToken.Reference] =
    body
      .map { b =>
        def parseBodyFrom(idx: Int): List[DocToken.Reference] = {
          DocToken.referenceParser.parse(b, idx) match {
            case Parsed.Success(value, index) =>
              List(DocToken.Reference(value)) ++ parseBodyFrom(index)
            case _ => List[DocToken.Reference]()
          }
        }

        parseBodyFrom(0)
      }
      .getOrElse(Nil)
}

/**
  * Companion object containing the classes required for describing an ScalaDoc token.
  *
  * The available tokens and their documentation are obtained from:
  * @see http://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html
  */
object DocToken {

  /**
    * Parser that for obtaining a class reference from an scaladoc body.
    */
  private val referenceParser: Parser[String] = P(
    // Removes the elements previous to the references.
    ((AnyChar ~ !"[[").rep ~ AnyChar).?
    // Retrieves the element within a
      ~ "[[" ~
      ((AnyChar ~ !"]]").rep ~ AnyChar).!
      ~ "]]"
  )

  /**
    * Represents an documentation code reference.
    */
  case class Reference(to: String) extends AnyVal {

    override def toString: String = s"[[$to]]"
  }

  /**
    * Returns all the labelled token kinds.
    */
  val tagTokenKinds: List[TagKind] =
    List(
      Constructor,
      Param,
      TypeParam,
      Return,
      Throws,
      See,
      Note,
      Example,
      UseCase,
      Author,
      Version,
      Since,
      Todo,
      Deprecated,
      Migration,
      Group,
      GroupName,
      GroupDescription,
      GroupPriority,
      Documentable
    )

  /**
    * Represents a documentation remark.
    */
  sealed abstract class Kind

  /**
    * Represents a labeled documentation remark.
    */
  sealed abstract class TagKind(val label: String, val numberParameters: Int) extends Kind

  /**
    * Helper [[DocToken]] apply method.
    */
  def apply(kind: Kind): DocToken = new DocToken(kind, None, None)

  /**
    * Helper [[DocToken]] apply method.
    */
  def apply(kind: Kind, body: String): DocToken = new DocToken(kind, None, Option(body))

  /**
    * Helper apply method for named [[DocToken]].
    */
  def apply(kind: TagKind, name: String, body: String): DocToken =
    new DocToken(kind, Option(name), Option(body))

  /**
    * Companion object for [[TagKind]] containing
    * its pattern match extractor.
    */
  object TagKind {
    def unapply(kind: TagKind): Option[(String, Int)] =
      Option(kind.label, kind.numberParameters)
  }

  /**
    * Placed in the class comment will describe the primary constructor.
    */
  case object Constructor extends TagKind("@constructor", numberParameters = 1)

  /**
    * Documents a specific value parameter of a method or class constructor.
    */
  case object Param extends TagKind("@param", numberParameters = 2)

  /**
    * Documents a specific type parameter of a method, class, trait or abstract type.
    */
  case object TypeParam extends TagKind("@tparam", numberParameters = 2)

  /**
    * Documents the return value of a method.
    */
  case object Return extends TagKind("@returns", numberParameters = 1)

  /**
    * Documents an exception type that may be thrown by a method or class constructor.
    */
  case object Throws extends TagKind("@throws", numberParameters = 2)

  /**
    * Points to other sources of information such as external documentation
    * or related entities in the documentation.
    */
  case object See extends TagKind("@see", numberParameters = 1)

  /**
    * Documents pre- and post-conditions as well as other notable requirements
    * or restrictions.
    */
  case object Note extends TagKind("@note", numberParameters = 1)

  /**
    * Provides example code and related descriptions.
    */
  case object Example extends TagKind("@example", numberParameters = 1)

  /**
    * Documents a use case of a method, class, trait or abstract type.
    */
  case object UseCase extends TagKind("@usecase", numberParameters = 1)

  /**
    * Document an entity to one author.
    */
  case object Author extends TagKind("@author", numberParameters = 1)

  /**
    * The version of the system or API that a class, trait, object or
    * package is part of.
    */
  case object Version extends TagKind("@version", numberParameters = 1)

  /**
    * The version of the system or API that an entity was first defined in.
    */
  case object Since extends TagKind("@since", numberParameters = 1)

  /**
    * Documents unimplemented features in an entity.
    */
  case object Todo extends TagKind("@todo", numberParameters = 1)

  /**
    * Marks an entity as deprecated. The message should
    * describe replacement implementation.
    */
  case object Deprecated extends TagKind("@deprecated", numberParameters = 1)

  /**
    * Like [[Deprecated]] but provides advanced warning of
    * planned changes ahead of deprecation.
    */
  case object Migration extends TagKind("@migration", numberParameters = 1)

  /**
    * Marks the entity as member of the body group.
    */
  case object Group extends TagKind("@group", numberParameters = 1)

  /**
    * Provide an optional name for the group.
    */
  case object GroupName extends TagKind("@groupname", numberParameters = 2)

  /**
    * Adds an optional descriptive text to display under the group name.
    */
  case object GroupDescription extends TagKind("@groupdesc", numberParameters = 2)

  /**
    * Control the order of the group on the page. Defaults to 0. Ungrouped
    * elements have an implicit priority of 1000. Use a value between 0 and
    * 999 to set a relative position to other groups. Low values will appear
    * before high values.
    */
  case object GroupPriority extends TagKind("@groupprio", numberParameters = 1)

  /**
    * Expand a type alias and abstract type into a full template page.
    */
  case object Documentable extends TagKind("@documentable", numberParameters = 1)

  /**
    * Represents an empty scaladoc line.
    */
  case object Paragraph extends Kind

  /**
    * Take comments from a superclass as defaults if comments
    * are not provided locally.
    */
  case object InheritDoc extends Kind

  /**
    * Represents an scaladoc code block.
    */
  case object CodeBlock extends Kind

  /**
    * Represents an unknown tag.
    */
  case object OtherTag extends Kind

  /**
    * Documents an untagged scaladoc description.
    */
  case object Description extends Kind

  /**
    * Documents a Scaladoc Heading.
    */
  abstract class Heading(val level: Int) extends Kind

  /**
    * Represents a first level heading:
    *
    * i.e: '=HEADING='
    */
  case object Heading1 extends Heading(1)

  /**
    * Represents a second level heading:
    *
    * i.e: '==HEADING=='
    */
  case object Heading2 extends Heading(2)

  /**
    * Represents a third level heading:
    *
    * i.e: '===HEADING==='
    */
  case object Heading3 extends Heading(3)

  /**
    * Represents a fourth level heading:
    *
    * i.e: '====HEADING===='
    */
  case object Heading4 extends Heading(4)

  /**
    * Represents a fifth level heading:
    *
    * i.e: '=====HEADING====='
    */
  case object Heading5 extends Heading(5)

  /**
    * Represents a sixth level heading:
    *
    * i.e: '======HEADING======'
    */
  case object Heading6 extends Heading(6)

  /**
    * Contains all the implemented [[Heading]]'s.
    */
  def allHeadings: Seq[Heading] = List(Heading1, Heading2, Heading3, Heading4, Heading5, Heading6)

  /**
    * Obtains a heading by its level if available.
    */
  def headingForLevel(level: Int): Option[Heading] = allHeadings.find(_.level == level)

}
