package scala.meta
package internal

package object parsers {
  // Materializes an instance of a given type, i.e.:
  //   * For a module type, returns the instance of the module.
  //   * For a class type with a default constructor, instantiates an instance of the class.
  //   * For other types, produces a static error.
  //
  // NOTE: Only used in the DSL for token classification employed in ScalametaParser.
  def instanceOf[T: InstanceTag] = implicitly[InstanceTag[T]].instantiate
}
