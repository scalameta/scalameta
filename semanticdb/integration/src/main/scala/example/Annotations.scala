package annot

import scala.language.experimental.macros

import com.javacp.annot._

@ClassAnnotation
class Annotations[@TypeParameterAnnotation T](@ParameterAnnotation x: T) { self: AnyRef =>
  @FieldAnnotation
  val field = 42

  @MethodAnnotation
  def method = {
    @LocalAnnotation
    val local = 42
    local
  }
  @TypeAnnotation
  type T
}

class B @ConstructorAnnotation()(x: Int) {
  @ConstructorAnnotation
  def this() = this(42)
}

@ObjectAnnotation
object M {
  @MacroAnnotation
  def m[TT] = macro ???
}

@TraitAnnotation
trait T
