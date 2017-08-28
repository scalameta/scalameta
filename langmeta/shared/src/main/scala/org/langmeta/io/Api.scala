package org.langmeta
package io

import org.langmeta
private[langmeta] trait Api {
}

private[langmeta] trait Aliases {
  type AbsolutePath = org.langmeta.io.AbsolutePath
  lazy val AbsolutePath = org.langmeta.io.AbsolutePath

  type RelativePath = org.langmeta.io.RelativePath
  lazy val RelativePath = org.langmeta.io.RelativePath

  type Multipath = org.langmeta.io.Multipath
  // there's no term Multipath, so we don't have a term alias here

  type Fragment = org.langmeta.io.Fragment
  val Fragment = org.langmeta.io.Fragment

  type Classpath = org.langmeta.io.Classpath
  lazy val Classpath = org.langmeta.io.Classpath

  type Sourcepath = org.langmeta.io.Sourcepath
  lazy val Sourcepath = org.langmeta.io.Sourcepath
}
