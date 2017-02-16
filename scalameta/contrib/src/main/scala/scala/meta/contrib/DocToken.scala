package scala.meta.contrib

import scala.meta.contrib.DocToken.DocKind

/**
  * Represents a scaladoc line.
  */
case class DocToken(kind: DocKind, name: Option[String], body: String) {

  override def toString: String = {
    (name match {
      case Some(n) => s"$kind(name=$n, body=$body)"
      case _ => s"$kind($body)"
    }).replaceAll("\n", " ")
  }

  /**
    * Appends a this [[DocToken#body]] to the input [[DocToken#body]].
    */
  def append(docToken: DocToken) : DocToken = copy(body = s"$body\n${docToken.body}")
}

/**
  * Companion object containing the classes required for describing an ScalaDoc token.
  *
  * The available tokens and their documentation are obtained from:
  * @see http://docs.scala-lang.org/overviews/scaladoc/for-library-authors.html
  */
object DocToken {

  /**
    * Helper [[DocToken]] apply method.
    */
  def apply(kind: DocKind, body: String): DocToken =
    new DocToken(kind, None, body)

  /**
    * Helper apply method for named [[DocToken]].
    */
  def apply(kind: DocKind, name: String, body: String): DocToken =
    new DocToken(kind, Option(name), body)

  /**
    * Trait used for representing each type of documentation label.
    */
  sealed trait DocKind {

    /**
      * Scaladoc label of the documentation kind.
      */
    val label: String
  }

  /**
    * Placed in the class comment will describe the primary constructor.
    */
  case object DocConstructor extends DocKind {

    override val label: String = "@constructor"
  }

  /**
    * Documents a specific value parameter of a method or class constructor.
    */
  case object DocParam extends DocKind {

    override val label: String = "@param"
  }

  /**
    * Documents a specific type parameter of a method, class, trait or abstract type.
    */
  case object DocTypeParam extends DocKind {

    override val label: String = "@tparam"
  }

  /**
    * Documents the return value of a method.
    */
  case object DocReturn extends DocKind {

    override val label: String = "@returns"
  }

  /**
    * Documents an exception type that may be thrown by a method or class constructor.
    */
  case object DocThrows extends DocKind {

    override val label: String = "@throws"
  }

  /**
    * Points to other sources of information such as external documentation
    * or related entities in the documentation.
    */
  case object DocSee extends DocKind {

    override val label: String = "@see"
  }

  /**
    * Documents pre- and post-conditions as well as other notable requirements
    * or restrictions.
    */
  case object DocNote extends DocKind {

    override val label: String = "@note"
  }

  /**
    * Provides example code and related descriptions.
    */
  case object DocExample extends DocKind {

    override val label: String = "@example"
  }

  /**
    * Documents a use case of a method, class, trait or abstract type.
    */
  case object DocUseCase extends DocKind {

    override val label: String = "@usecase"
  }

  /**
    * Attributes an entity to one author.
    */
  case object DocAuthor extends DocKind {

    override val label: String = "@author"
  }

  /**
    * The version of the system or API that a class, trait, object or
    * package is part of.
    */
  case object DocVersion extends DocKind {

    override val label: String = "@version"
  }

  /**
    * The version of the system or API that an entity was first defined in.
    */
  case object DocSince extends DocKind {

    override val label: String = "@since"
  }

  /**
    * Documents unimplemented features in an entity.
    */
  case object DocTodo extends DocKind {

    override val label: String = "@todo"
  }

  /**
    * Marks an entity as deprecated. The message should
    * describe replacement implementation.
    */
  case object DocDeprecated extends DocKind {

    override val label: String = "@deprecated"
  }

  /**
    * Like [[DocDeprecated]] but provides advanced warning of
    * planned changes ahead of deprecation.
    */
  case object DocMigration extends DocKind {

    override val label: String = "@migration"
  }

  /**
    * Marks the entity as member of the body group.
    */
  case object DocGroup extends DocKind {

    override val label: String = "@group"
  }

  /**
    * Provide an optional name for the group.
    */
  case object DocGroupName extends DocKind {

    override val label: String = "@groupname"
  }

  /**
    * Adds an optional descriptive text to display under the group name.
    */
  case object DocGroupDescription extends DocKind {

    override val label: String = "@groupdesc"
  }

  /**
    * Control the order of the group on the page. Defaults to 0. Ungrouped
    * elements have an implicit priority of 1000. Use a value between 0 and
    * 999 to set a relative position to other groups. Low values will appear
    * before high values.
    */
  case object DocGroupPriority extends DocKind {

    override val label: String = "@groupprio"
  }

  /**
    * Expand a type alias and abstract type into a full template page.
    */
  case object DocDocumentable extends DocKind {

    override val label: String = "@documentable"
  }

  /**
    * Take comments from a superclass as defaults if comments
    * are not provided locally.
    */
  case object DocInheritDoc extends DocKind {

    override val label: String = "@inheritdoc"
  }

  /**
    * Represents an unknown tag.
    */
  case object DocOtherTag extends DocKind {
    override val label: String = "@"
  }

  /**
    * Documents an untagged scaladoc description.
    */
  case object DocText extends DocKind {

    override val label: String = ""
  }

}
