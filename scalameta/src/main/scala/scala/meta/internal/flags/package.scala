package scala.meta
package internal

package object flags {
  type Flags = Int
  final val ZERO: Flags = 0x0
  final val TYPECHECKED: Flags = 0x1
}