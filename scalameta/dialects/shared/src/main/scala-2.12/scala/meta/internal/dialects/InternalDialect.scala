package scala.meta
package internal
package dialects

import scala.meta.dialects.Scala212

trait InternalDialect {
  // NOTE: See https://github.com/scalameta/scalameta/issues/253 for discussion.
  implicit def current: Scala212.type = Scala212
}
