// NOTE: copy/pasted from https://github.com/lampepfl/dotty/tree/1962ada58fcd2333a2e40179ab0ac6efb6167ed2

package scala.meta
package internal
package tasty

import collection.mutable

abstract class TastyName

object TastyName {

  case class NameRef(index: Int) extends AnyVal

  case class Simple(name: String) extends TastyName
  case class Qualified(qualified: NameRef, selector: NameRef) extends TastyName
  case class Signed(original: NameRef, params: List[NameRef], result: NameRef) extends TastyName
  case class Expanded(prefix: NameRef, original: NameRef) extends TastyName
  case class ModuleClass(module: NameRef) extends TastyName
  case class SuperAccessor(accessed: NameRef) extends TastyName
  case class DefaultGetter(method: NameRef, num: Int) extends TastyName
  case class Shadowed(original: NameRef) extends TastyName

  class Table extends (NameRef => TastyName) {
    private val names = new mutable.ArrayBuffer[TastyName]
    def add(name: TastyName) = names += name
    def apply(ref: NameRef) = names(ref.index)
    def contents: Iterable[TastyName] = names
  }
}
