package org.langmeta.internal.semanticdb {
  trait Api extends org.langmeta.semanticdb.Api
  trait Aliases extends org.langmeta.semanticdb.Aliases
}

package scala.meta.semanticdb {
  private[meta] trait Api extends org.langmeta.internal.semanticdb.Api
  private[meta] trait Aliases extends org.langmeta.internal.semanticdb.Aliases
}
