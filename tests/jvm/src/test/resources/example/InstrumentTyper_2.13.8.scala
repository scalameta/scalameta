package example

import scala.annotation.meta.param/*=>scala.annotation.meta.param#*/
import scala.language/*=>scala.language.*/.existentials/*=>scala.language.existentials.*/
import scala.language/*=>scala.language.*/.higherKinds/*=>scala.language.higherKinds.*/
import types.Test/*=>types.Test.*/._

class InstrumentTyper/*<=example.InstrumentTyper#*/ { self/*<=local0*/: AnyRef =>
  def all/*<=example.InstrumentTyper#all().*/ = List/*=>scala.collection.immutable.List.*/(
    Literal/*=>types.Test.Literal.*/.int/*=>types.Test.Literal.int.*/,
    Literal/*=>types.Test.Literal.*/.long/*=>types.Test.Literal.long.*/,
    Literal/*=>types.Test.Literal.*/.float/*=>types.Test.Literal.float.*/,
    Literal/*=>types.Test.Literal.*/.double/*=>types.Test.Literal.double.*/,
    Literal/*=>types.Test.Literal.*/.nil/*=>types.Test.Literal.nil.*/,
    Literal/*=>types.Test.Literal.*/.char/*=>types.Test.Literal.char.*/,
    Literal/*=>types.Test.Literal.*/.string/*=>types.Test.Literal.string.*/,
    Literal/*=>types.Test.Literal.*/.bool/*=>types.Test.Literal.bool.*/,
    Literal/*=>types.Test.Literal.*/.unit/*=>types.Test.Literal.unit.*/,
    Literal/*=>types.Test.Literal.*/.javaEnum/*=>types.Test.Literal.javaEnum.*/,
    Literal/*=>types.Test.Literal.*/.clazzOfInt/*=>types.Test.Literal.clazzOfInt.*/,
    Literal/*=>types.Test.Literal.*/.clazzOfOption/*=>types.Test.Literal.clazzOfOption.*/,
    List/*=>scala.collection.immutable.Nil.*/()
  )
  def existential/*<=example.InstrumentTyper#existential().*/: U/*=>local1*/[Int/*=>scala.Int#*/] forSome { type U/*<=local1*/[T/*<=local2*/ <: Int] } = ???/*=>scala.Predef.`???`().*/
  type AnnotatedType/*<=example.InstrumentTyper#AnnotatedType#*/ = Int/*=>scala.Int#*/ @param
  def singletonType/*<=example.InstrumentTyper#singletonType().*/(x/*<=example.InstrumentTyper#singletonType().(x)*/: Predef/*=>scala.Predef.*/.type) = ???/*=>scala.Predef.`???`().*/
  final val clazzOf/*<=example.InstrumentTyper#clazzOf.*/ = classOf/*=>scala.Predef.classOf().*/[Option[Int]]
}
