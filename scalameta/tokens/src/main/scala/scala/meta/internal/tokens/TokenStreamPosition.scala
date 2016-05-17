package scala.meta
package internal
package tokens

import org.scalameta.data._
import scala.meta.inputs._

// NOTE: `start` and `end` are not characters offsets,
// but token indices in the underlying token stream.
// Also `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore TokenStreamPosition.end can point to the last token plus one.
@data class TokenStreamPosition(start: Int, end: Int) extends Serializable
