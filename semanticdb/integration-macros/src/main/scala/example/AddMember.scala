package example

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

/**
 * Keeps the annotated object and adds `anon`, an anonymous class, and `call`, a call of
 * [[AnonymousExpansion]].
 */
class AddMember extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AddMember.impl
}

object AddMember {
  def impl(c: blackbox.Context)(annottees: c.Tree*): c.Tree = {
    import c.universe._
    val q"$mods object $name extends { ..$early } with ..$parents { $self => ..$stats }" =
      annottees.head
    val stats1 = stats :+ q"def anon = new AnyRef { def z = 1 }" :+
      q"def call = _root_.example.AnonymousExpansion()"
    q"$mods object $name extends { ..$early } with ..$parents { $self => ..$stats1 }"
  }
}
