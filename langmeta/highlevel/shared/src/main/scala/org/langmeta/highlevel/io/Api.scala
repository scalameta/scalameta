package org.langmeta.highlevel.io

private[langmeta] trait Api {
}

private[langmeta] trait Aliases {
  type AbsolutePath = org.langmeta.highlevel.io.AbsolutePath
  lazy val AbsolutePath = org.langmeta.highlevel.io.AbsolutePath

  type RelativePath = org.langmeta.highlevel.io.RelativePath
  lazy val RelativePath = org.langmeta.highlevel.io.RelativePath

  type Multipath = org.langmeta.highlevel.io.Multipath
  // there's no term Multipath, so we don't have a term alias here

  type Fragment = org.langmeta.highlevel.io.Fragment
  val Fragment = org.langmeta.highlevel.io.Fragment

  type Classpath = org.langmeta.highlevel.io.Classpath
  lazy val Classpath = org.langmeta.highlevel.io.Classpath

  type Sourcepath = org.langmeta.highlevel.io.Sourcepath
  lazy val Sourcepath = org.langmeta.highlevel.io.Sourcepath
}
