package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/** On a parameter, returns the parameter's owner and its companion as scalac passes them. */
class KeepParamOwner extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro KeepParamOwner.impl
}

object KeepParamOwner {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    // the parameter comes first, then its owner and the owner's companion
    q"..${annottees.tail}"
  }
}
