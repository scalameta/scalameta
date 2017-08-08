package scala.meta.internal

import scala.language.implicitConversions

package object semantic {
  implicit def XtensionSchemaDatabase(sdatabase: lang.meta.internal.semanticdb.schema.Database) =
    new lang.meta.internal.semanticdb.XtensionSchemaDatabase(sdatabase)
  implicit def XtensionDatabase(ddatabase: lang.meta.semanticdb.Database) =
    new lang.meta.internal.semanticdb.XtensionDatabase(ddatabase)

  object vfs {
    val Database = lang.meta.internal.semanticdb.vfs.Database
    type Database = lang.meta.internal.semanticdb.vfs.Database
    val Entry = lang.meta.internal.semanticdb.vfs.Entry
    type Entry = lang.meta.internal.semanticdb.vfs.Entry
    val SemanticdbPaths = lang.meta.internal.semanticdb.vfs.SemanticdbPaths
    // there's no type SemanticdbPaths, so we don't have a type alias here
  }

  object schema {
    val Attributes = lang.meta.internal.semanticdb.schema.Attributes
    type Attributes = lang.meta.internal.semanticdb.schema.Attributes
    // TODO
  }
}