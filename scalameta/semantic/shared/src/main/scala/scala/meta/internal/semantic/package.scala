package scala.meta.internal

import scala.language.implicitConversions

package object semantic {
  implicit def XtensionSchemaDatabase(sdatabase: star.meta.internal.semanticdb.schema.Database) = new star.meta.internal.semanticdb.XtensionSchemaDatabase(sdatabase)
  implicit def XtensionDatabase(ddatabase: star.meta.semanticdb.Database) = new star.meta.internal.semanticdb.XtensionDatabase(ddatabase)

  val DatabaseSyntax = star.meta.internal.semanticdb.DatabaseSyntax
  val AttributesSyntax = star.meta.internal.semanticdb.AttributesSyntax
  val FlagSyntax = star.meta.internal.semanticdb.FlagSyntax

  object vfs {
    val Database = star.meta.internal.semanticdb.vfs.Database
    type Database = star.meta.internal.semanticdb.vfs.Database
    val Entry = star.meta.internal.semanticdb.vfs.Entry
    type Entry = star.meta.internal.semanticdb.vfs.Entry
    val SemanticdbPaths = star.meta.internal.semanticdb.vfs.SemanticdbPaths
    // there's no type SemanticdbPaths, so we don't have a type alias here
  }

  object schema {
    // TODO
  }
}