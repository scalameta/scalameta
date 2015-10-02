package scala.meta
package interactive

import scala.{Seq => _}
import scala.collection.immutable.Seq
import org.scalameta.annotations._
import scala.annotation._
import scala.meta.artifacts.Artifact

// TODO: This API needs to be expanded, and I think it might be a really good idea to do that,
// because then we could potentially unite the currently disparate IDE and REPL mechanisms,
// which are essentially about the same thing - analyzing partially correct syntax trees
// and then loading some of them into a workspace. This would allow both REPL and IDE benefit
// from developments in either of those (better auto-completion, for instance).
//
// In order for this to become reality, we'd need:
// 1) Special XXX.Error AST nodes (this is simple, because we already support this notion with Quasis),
// 2) Support for error nodes in the parser (no idea how complicated this is going to be),
// 3) Figure out how to merge CompilerControl and Pressy.

@opaque
@implicitNotFound("this method requires an implicit scala.meta.interactive.Context")
trait Context {
  def load(artifacts: Seq[Artifact]): Seq[Artifact]
}