package scala.meta
package quasiquotes

import scala.meta.parsers._
import scala.meta.internal.parsers.ScalametaParser.toParse

// NOTE: It would be nice to have quasiquotes delegate to regular parser routines,
// but for some corner cases of the language this doesn't quite work,
// so we have to work around a bit. Maybe in the future we discover a better approach.
private[meta] trait QuasiquoteParsers extends scala.meta.parsers.Api {
  implicit lazy val parseQuasiquoteStat: Parse[Stat] = toParse(_.parseQuasiquoteStat())
  implicit lazy val parseQuasiquoteCtor: Parse[Ctor] = toParse(_.parseQuasiquoteCtor())
  implicit lazy val parseQuasiquotePat: Parse[Pat] = toParse(_.parseQuasiquotePat())
  implicit lazy val parseQuasiquotePatArg: Parse[Pat.Arg] = toParse(_.parseQuasiquotePatArg())
  implicit lazy val parseQuasiquotePatType: Parse[Pat.Type] = toParse(_.parseQuasiquotePatType())
  implicit lazy val parseQuasiquoteMod: Parse[Mod] = toParse(_.parseQuasiquoteMod())
}