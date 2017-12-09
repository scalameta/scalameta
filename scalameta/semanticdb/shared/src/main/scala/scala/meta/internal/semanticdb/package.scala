package scala.meta.internal

import scala.language.implicitConversions

package object semanticdb {
  implicit def XtensionSchemaDatabase(sdatabase: org.langmeta.lowlevel.semanticdb.Database) =
    new org.langmeta.internal.semanticdb.XtensionSchemaDatabase(sdatabase)
  implicit def XtensionDatabase(ddatabase: org.langmeta.highlevel.semanticdb.Database) =
    new org.langmeta.internal.semanticdb.XtensionDatabase(ddatabase)

  object vfs {
    val Database = org.langmeta.internal.semanticdb.vfs.Database
    type Database = org.langmeta.internal.semanticdb.vfs.Database
    val Entry = org.langmeta.internal.semanticdb.vfs.Entry
    type Entry = org.langmeta.internal.semanticdb.vfs.Entry
    val SemanticdbPaths = org.langmeta.internal.semanticdb.vfs.SemanticdbPaths
    // there's no type SemanticdbPaths, so we don't have a type alias here
  }

  object schema {
    val Document = org.langmeta.lowlevel.semanticdb.Document
    type Document = org.langmeta.lowlevel.semanticdb.Document
    // TODO
  }
}
