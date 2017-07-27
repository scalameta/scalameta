package star.meta
package io

private[meta] trait Api {
}

private[meta] trait Aliases {
  type AbsolutePath = star.meta.io.AbsolutePath
  lazy val AbsolutePath = star.meta.io.AbsolutePath

  type RelativePath = star.meta.io.RelativePath
  lazy val RelativePath = star.meta.io.RelativePath

  type Multipath = star.meta.io.Multipath
  // there's no term Multipath, so we don't have a term alias here

  type Fragment = star.meta.io.Fragment
  val Fragment = star.meta.io.Fragment

  type Classpath = star.meta.io.Classpath
  lazy val Classpath = star.meta.io.Classpath

  type Sourcepath = star.meta.io.Sourcepath
  lazy val Sourcepath = star.meta.io.Sourcepath
}
