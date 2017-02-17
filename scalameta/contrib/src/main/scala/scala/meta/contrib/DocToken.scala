package scala.meta.contrib

import scala.meta.contrib.DocToken.Kind

/**
  * Represents a scaladoc line.
  */
case class DocToken(kind: Kind, name: Option[String], body: String) {

  override def toString: String = {
    (name match {
      case Some(n) => s"$kind(name=$n, body=$body)"
      case _ => s"$kind($body)"
    }).replaceAll("\n", " ")
  }

  /**
    * Appends a this [[DocToken#body]] to the input [[DocToken#body]].
    */
  def append(appendedBody: String) : DocToken = copy(body = s"$body\n$appendedBody")
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
  val labelledTokenKinds = Seq(
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
    Documentable,
    InheritDoc
  )

  /**
    * Helper [[DocToken]] apply method.
    */
  def apply(kind: Kind, body: String): DocToken =
    new DocToken(kind, None, body)

  /**
    * Helper apply method for named [[DocToken]].
    */
  def apply(kind: Kind, name: String, body: String): DocToken =
    new DocToken(kind, Option(name), body)

  /**
    * Trait used for representing each type of documentation label.
    */
  sealed abstract class Kind(val label: String, val numberParameters: Int)

  /**
    * Placed in the class comment will describe the primary constructor.
    */
  case object Constructor extends Kind("@constructor", numberParameters = 1)

  /**
    * Documents a specific value parameter of a method or class constructor.
    */
  case object Param extends Kind("@param", numberParameters = 2)

  /**
    * Documents a specific type parameter of a method, class, trait or abstract type.
    */
  case object TypeParam extends Kind("@tparam", numberParameters = 2)

  /**
    * Documents the return value of a method.
    */
  case object Return extends Kind("@returns", numberParameters = 1)

  /**
    * Documents an exception type that may be thrown by a method or class constructor.
    */
  case object Throws extends Kind("@throws", numberParameters = 2)

  /**
    * Points to other sources of information such as external documentation
    * or related entities in the documentation.
    */
  case object See extends Kind("@see", numberParameters = 1)

  /**
    * Documents pre- and post-conditions as well as other notable requirements
    * or restrictions.
    */
  case object Note extends Kind("@note", numberParameters = 1)

  /**
    * Provides example code and related descriptions.
    */
  case object Example extends Kind("@example", numberParameters = 1)

  /**
    * Documents a use case of a method, class, trait or abstract type.
    */
  case object UseCase extends Kind("@usecase", numberParameters = 1)

  /**
    * Attributes an entity to one author.
    */
  case object Author extends Kind("@author", numberParameters = 1)

  /**
    * The version of the system or API that a class, trait, object or
    * package is part of.
    */
  case object Version extends Kind("@version", numberParameters = 1)

  /**
    * The version of the system or API that an entity was first defined in.
    */
  case object Since extends Kind("@since", numberParameters = 1)

  /**
    * Documents unimplemented features in an entity.
    */
  case object Todo extends Kind("@todo", numberParameters = 1)

  /**
    * Marks an entity as deprecated. The message should
    * describe replacement implementation.
    */
  case object Deprecated extends Kind("@deprecated", numberParameters = 1)

  /**
    * Like [[Deprecated]] but provides advanced warning of
    * planned changes ahead of deprecation.
    */
  case object Migration extends Kind("@migration", numberParameters = 1)

  /**
    * Marks the entity as member of the body group.
    */
  case object Group extends Kind("@group", numberParameters = 1)

  /**
    * Provide an optional name for the group.
    */
  case object GroupName extends Kind("@groupname", numberParameters = 2)

  /**
    * Adds an optional descriptive text to display under the group name.
    */
  case object GroupDescription extends Kind("@groupdesc", numberParameters = 2)

  /**
    * Control the order of the group on the page. Defaults to 0. Ungrouped
    * elements have an implicit priority of 1000. Use a value between 0 and
    * 999 to set a relative position to other groups. Low values will appear
    * before high values.
    */
  case object GroupPriority extends Kind("@groupprio", numberParameters = 1)

  /**
    * Expand a type alias and abstract type into a full template page.
    */
  case object Documentable extends Kind("@documentable", numberParameters = 1)

  /**
    * Take comments from a superclass as defaults if comments
    * are not provided locally.
    */
  case object InheritDoc extends Kind("@inheritdoc", numberParameters = 0)

  /**
    * Represents an unknown tag.
    */
  case object OtherTag extends Kind("@", numberParameters = 1)

  /**
    * Documents an untagged scaladoc description.
    */
  case object Description extends Kind("", numberParameters = 1)

}
