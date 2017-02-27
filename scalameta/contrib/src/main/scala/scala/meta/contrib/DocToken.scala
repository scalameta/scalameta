package scala.meta.contrib

import DocToken.Kind

/**
  * Represents a scaladoc line.
  */
case class DocToken(kind: Kind, name: Option[String], body: Option[String]) {

  override def toString: String = {
    ((name, body) match {
      case (Some(n), _) => s"$kind(name=$n, body=${body.getOrElse("")})"
      case (None, Some(kindBody)) => s"$kind($kindBody)"
      case _ => kind.toString
    }).replaceAll("\n", " ")
  }
}

/**
  * Companion object containing the classes required for describing an ScalaDoc token.
  *
  * The available tokens and their documentation are obtained from:
  * @see http://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html
  */
object DocToken {

  /**
    * Returns all the labelled token kinds.
    */
  val labelledTokenKinds: Seq[LabelledKind] =
    Seq(
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
  def apply(kind: LabelledKind, name: String, body: String): DocToken =
    new DocToken(kind, Option(name), Option(body))

  /**
    * Represents a documentation remark.
    */
  sealed abstract class Kind

  /**
    * Represents a labeled documentation remark.
    */
  sealed abstract class LabelledKind(val label: String, val numberParameters: Int) extends Kind

  /**
    * Companion object for [[LabelledKind]] containing
    * its pattern match extractor.
    */
  object LabelledKind {
    def unapply(kind: LabelledKind): Option[(String, Int)] =
      Option(kind.label, kind.numberParameters)
  }

  /**
    * Placed in the class comment will describe the primary constructor.
    */
  case object Constructor extends LabelledKind("@constructor", numberParameters = 1)

  /**
    * Documents a specific value parameter of a method or class constructor.
    */
  case object Param extends LabelledKind("@param", numberParameters = 2)

  /**
    * Documents a specific type parameter of a method, class, trait or abstract type.
    */
  case object TypeParam extends LabelledKind("@tparam", numberParameters = 2)

  /**
    * Documents the return value of a method.
    */
  case object Return extends LabelledKind("@returns", numberParameters = 1)

  /**
    * Documents an exception type that may be thrown by a method or class constructor.
    */
  case object Throws extends LabelledKind("@throws", numberParameters = 2)

  /**
    * Points to other sources of information such as external documentation
    * or related entities in the documentation.
    */
  case object See extends LabelledKind("@see", numberParameters = 1)

  /**
    * Documents pre- and post-conditions as well as other notable requirements
    * or restrictions.
    */
  case object Note extends LabelledKind("@note", numberParameters = 1)

  /**
    * Provides example code and related descriptions.
    */
  case object Example extends LabelledKind("@example", numberParameters = 1)

  /**
    * Documents a use case of a method, class, trait or abstract type.
    */
  case object UseCase extends LabelledKind("@usecase", numberParameters = 1)

  /**
    * Attributes an entity to one author.
    */
  case object Author extends LabelledKind("@author", numberParameters = 1)

  /**
    * The version of the system or API that a class, trait, object or
    * package is part of.
    */
  case object Version extends LabelledKind("@version", numberParameters = 1)

  /**
    * The version of the system or API that an entity was first defined in.
    */
  case object Since extends LabelledKind("@since", numberParameters = 1)

  /**
    * Documents unimplemented features in an entity.
    */
  case object Todo extends LabelledKind("@todo", numberParameters = 1)

  /**
    * Marks an entity as deprecated. The message should
    * describe replacement implementation.
    */
  case object Deprecated extends LabelledKind("@deprecated", numberParameters = 1)

  /**
    * Like [[Deprecated]] but provides advanced warning of
    * planned changes ahead of deprecation.
    */
  case object Migration extends LabelledKind("@migration", numberParameters = 1)

  /**
    * Marks the entity as member of the body group.
    */
  case object Group extends LabelledKind("@group", numberParameters = 1)

  /**
    * Provide an optional name for the group.
    */
  case object GroupName extends LabelledKind("@groupname", numberParameters = 2)

  /**
    * Adds an optional descriptive text to display under the group name.
    */
  case object GroupDescription extends LabelledKind("@groupdesc", numberParameters = 2)

  /**
    * Control the order of the group on the page. Defaults to 0. Ungrouped
    * elements have an implicit priority of 1000. Use a value between 0 and
    * 999 to set a relative position to other groups. Low values will appear
    * before high values.
    */
  case object GroupPriority extends LabelledKind("@groupprio", numberParameters = 1)

  /**
    * Expand a type alias and abstract type into a full template page.
    */
  case object Documentable extends LabelledKind("@documentable", numberParameters = 1)

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

}
