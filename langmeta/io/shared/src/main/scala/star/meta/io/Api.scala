package lang.meta
package io

private[meta] trait Api {
}

private[meta] trait Aliases {
  type AbsolutePath = lang.meta.io.AbsolutePath
  lazy val AbsolutePath = lang.meta.io.AbsolutePath

  type RelativePath = lang.meta.io.RelativePath
  lazy val RelativePath = lang.meta.io.RelativePath

  type Multipath = lang.meta.io.Multipath
  // there's no term Multipath, so we don't have a term alias here

  type Fragment = lang.meta.io.Fragment
  val Fragment = lang.meta.io.Fragment

  type Classpath = lang.meta.io.Classpath
  lazy val Classpath = lang.meta.io.Classpath

  type Sourcepath = lang.meta.io.Sourcepath
  lazy val Sourcepath = lang.meta.io.Sourcepath
}
