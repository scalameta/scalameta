package scala.meta.tests
package trees

import scala.meta.{Defn, Template, Tree}

import scala.reflect.runtime.universe._

import munit._

/* a client compiled against an older release keeps linking: every shape of newBuilder stays on
 * the companion, every field's setter stays on the builder, old names included, and the comment
 * params keep their default getters */
class BuilderSurfaceSuite extends FunSuite {

  private def isDeprecated(m: Symbol) = m.annotations.exists(_.tree.tpe =:= typeOf[deprecated])

  // each shape of newBuilder, as its parameter types, with whether it is deprecated
  private def newBuilders(tpe: Type): Map[List[String], Boolean] = tpe.member(TermName("newBuilder"))
    .alternatives
    .map(m => m.asMethod.paramLists.head.map(_.info.typeSymbol.name.toString) -> isDeprecated(m))
    .toMap

  // each public method of the builder, with whether it is deprecated
  private def methods(tpe: Type): Map[String, Boolean] = tpe.decls.collect {
    case m: MethodSymbol if m.isPublic && !m.isConstructor && !m.isSynthetic =>
      m.name.toString -> isDeprecated(m)
  }.toMap

  test("Template.newBuilder")(assertEquals(
    newBuilders(typeOf[Template.type]),
    Map(List("Option", "List", "Body") -> false, List("List", "List", "Self", "List") -> true),
  ))

  test("Template.Builder") {
    val current =
      Set("earlyClause", "inits", "body", "derives", "origin", "begComment", "endComment")
    val old = Set("early", "self", "stats")
    val obtained = methods(typeOf[Template.Builder])
    assertEquals(obtained.filter(!_._2).keySet, current + "result")
    assertEquals(obtained.filter(_._2).keySet, old)
  }

  test("Defn.Def.newBuilder")(assertEquals(
    newBuilders(typeOf[Defn.Def.type]),
    Map(
      List("List", "Name", "List", "Option", "Term") -> false,
      List("List", "Name", "List", "List", "Option", "Term") -> true,
      List("List", "Name", "Option", "Option", "Term") -> true,
    ),
  ))

  // a field added after the comment fields must not shift their default getters
  private def checkCommentDefaults(cls: Class[_], companion: AnyRef, pos: Int): Unit = {
    def ret(c: Class[_], name: String) = c.getMethod(name).getReturnType.getName
    (pos to pos + 1).foreach { i =>
      assertEquals(ret(cls, s"copyWithComments$$default$$$i"), "scala.Option")
      assertEquals(ret(companion.getClass, s"createWithComments$$default$$$i"), "scala.Option")
    }
  }

  test("Tree.Comment")(checkCommentDefaults(classOf[Tree.Comment], Tree.Comment, 2))

  test("Tree.Comments")(checkCommentDefaults(classOf[Tree.Comments], Tree.Comments, 2))

}
