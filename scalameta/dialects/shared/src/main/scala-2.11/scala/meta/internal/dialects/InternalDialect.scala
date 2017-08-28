package scala.meta
package internal
package dialects

import scala.meta.dialects.Scala211

trait InternalDialect {
  // NOTE: See https://github.com/scalameta/scalameta/issues/253 for discussion.
  implicit def current: Scala211.type = Scala211
}
