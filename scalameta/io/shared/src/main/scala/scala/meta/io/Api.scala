package scala.meta
package io

private[meta] trait Api {
}

private[meta] trait Aliases {
  type AbsolutePath = scala.meta.io.AbsolutePath
  lazy val AbsolutePath = scala.meta.io.AbsolutePath

  type RelativePath = scala.meta.io.RelativePath
  lazy val RelativePath = scala.meta.io.RelativePath

  type Multipath = scala.meta.io.Multipath
  lazy val Multipath = scala.meta.io.Multipath

  type Fragment = scala.meta.io.Fragment
  lazy val Fragment = scala.meta.io.Fragment

  type Classpath = scala.meta.io.Classpath
  lazy val Classpath = scala.meta.io.Classpath
}
