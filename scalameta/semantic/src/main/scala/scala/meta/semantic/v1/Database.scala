package scala.meta
package semantic
package v1

// NOTE: This is an initial take on the semantic API.
// Instead of immediately implementing the full vision described in my dissertation,
// we will first deliver the long-hanging fruit (https://github.com/scalameta/scalameta/issues/604),
// and only then will approach really tricky tasks (https://github.com/scalameta/scalameta/issues/623).

// NOTE: `start` and `end` are String.substring-style,
// i.e. `start` is inclusive and `end` is not.
// Therefore Position.end can point to the last character plus one.
case class Location(uri: String, start: Int, end: Int)

case class Symbol(id: String)

case class Database(
  symbols: Map[Location, Symbol]
  // TODO: Additional fields are to be discussed
  // https://github.com/scalameta/scalameta/issues/605
)