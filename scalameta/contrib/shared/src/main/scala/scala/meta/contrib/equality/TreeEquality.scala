package scala.meta.contrib.equality

import scala.meta.Tree

/** Helper type used to help typechecker default to structural equality. */
trait TreeEquality[+A <: Tree]
