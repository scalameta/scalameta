package scala.meta.tests
package trees

import org.scalameta.invariants.InvariantFailedException
import scala.meta._
import scala.meta.inputs.Position
import scala.meta.trees.Origin

class BuilderSuite extends TreeSuiteBase {

  implicit val dialect: Dialect = dialects.Scala213

  private def comments(value: String) = Some(Tree.Comments(List(Tree.Comment(List(Lit.String(value))))))

  test("newBuilder: required fields") {
    val tree = Term.If.newBuilder(tname("a"), tname("b"), tname("c")).result()
    assertTree(tree)(Term.If(tname("a"), tname("b"), tname("c")))
    assert(tree.cond.parent.contains(tree))
    assert(tree.origin.dialectOpt.contains(dialects.Scala213))
  }

  test("newBuilder: a field with a default") {
    val tree = Term.If.newBuilder(tname("a"), tname("b"), tname("c")).mods(List(Mod.Inline()))
      .result()
    assertTree(tree)(Term.If(tname("a"), tname("b"), tname("c"), List(Mod.Inline())))
    assert(tree.mods.head.parent.contains(tree))
  }

  test("newBuilder: comments") {
    val tree = Term.Name.newBuilder("x").begComment(comments("/* b */")).endComment(comments("// e"))
      .result()
    assertStruct(tree)(
      """|/* b */
         |Term.Name("x") // e""".stripMargin,
    )
  }

  test("newBuilder: a null field") {
    intercept[InvariantFailedException](Term.Name.newBuilder(null))
    intercept[InvariantFailedException](Term.Name.newBuilder("x").value(null))
  }

  test("newBuilder: an empty field") {
    intercept[InvariantFailedException](Term.Name.newBuilder(""))
    intercept[InvariantFailedException](Term.Name.newBuilder("x").value(""))
  }

  test("Template: initial shape") {
    // early, inits, self, stats
    val expected = Template.Initial(List(int(0)), Nil, self("x"), List(int(1)))
    assertTree(Template.newBuilder(List(int(0)), Nil, self("x"), List(int(1))).result())(expected)
    val set = Template.newBuilder(Nil, Nil, self("y"), Nil).early(List(int(0))).self(self("x"))
      .stats(List(int(1))).result()
    assertTree(set)(expected)
  }

  test("Template: shape after 4.4.0") {
    // derives is added
    val expected = Template.After_4_4_0(List(int(0)), Nil, self("x"), List(int(1)), List(pname("T")))
    val built = Template.newBuilder(List(int(0)), Nil, self("x"), List(int(1)))
      .derives(List(pname("T"))).result()
    assertTree(built)(expected)
  }

  test("Template: shape after 4.9.9") {
    // earlyClause replaces early, and body replaces self and stats
    val early = Some(Stat.Block(List(int(0))))
    val expected = Template.After_4_9_9(early, Nil, tplBody("x", int(1)), List(pname("T")))
    val built = Template.newBuilder(early, Nil, tplBody("x", int(1))).derives(List(pname("T")))
      .result()
    assertTree(built)(expected)
    val copied = expected.toBuilder.stats(List(int(2))).result()
    assert(copied ne expected)
    assertTree(copied)(Template(early, Nil, tplBody("x", int(2)), List(pname("T"))))
    assertTree(expected)(Template.After_4_9_9(early, Nil, tplBody("x", int(1)), List(pname("T"))))
  }

  test("Defn.Def: initial shape") {
    // mods, name, tparams, paramss, decltpe, body
    val tp = pparam("A")
    val p = tparam("x", pname("Int"))
    val expected = Defn.Def.Initial(Nil, tname("f"), List(tp), List(List(p)), None, int(1))
    val built = Defn.Def.newBuilder(Nil, tname("f"), List(tp), List(List(p)), None, int(1)).result()
    assertTree(built)(expected)
    val set = Defn.Def.newBuilder(Nil, tname("f"), Nil, None, int(1)).tparams(List(tp))
      .paramss(List(List(p))).result()
    assertTree(set)(expected)
  }

  test("Defn.Def: shape after 4.6.0") {
    // paramClauseGroup replaces tparams and paramss
    val group = Some(pcg(Type.ParamClause(List(pparam("A"))), List(tparam("x", pname("Int")))))
    val expected = Defn.Def.After_4_6_0(Nil, tname("f"), group, None, int(1))
    assertTree(Defn.Def.newBuilder(Nil, tname("f"), group, None, int(1)).result())(expected)
    val set = Defn.Def.newBuilder(Nil, tname("f"), Nil, None, int(1)).paramClauseGroup(group).result()
    assertTree(set)(expected)
  }

  test("Defn.Def: shape after 4.7.3") {
    // paramClauseGroups replaces paramClauseGroup
    val groups = List(pcg(Type.ParamClause(List(pparam("A"))), List(tparam("x", pname("Int")))))
    val expected = Defn.Def.After_4_7_3(Nil, tname("f"), groups, None, int(1))
    assertTree(Defn.Def.newBuilder(Nil, tname("f"), groups, None, int(1)).result())(expected)
  }

  test("origin: a fresh tree") {
    val fresh = Term.Name.newBuilder("x").result()
    assertEquals(fresh.origin.dialectOpt, Some(dialects.Scala213))
    assertEquals(fresh.pos, Position.None)
    val parsed = dialect("a + xy").parse[Term].get.asInstanceOf[Term.ApplyInfix].args.head
    val tree = Term.Name.newBuilder("xy").origin(parsed.origin).result()
    assertEquals(tree.pos.start, 4)
    assertEquals(tree.pos.end, 6)
    assertEquals(tree.syntax, "xy")
  }

  test("origin: a copy") {
    val parsed = dialect("a + xy").parse[Term].get.asInstanceOf[Term.ApplyInfix].args.head
      .asInstanceOf[Term.Name]
    val copied = parsed.toBuilder.result()
    assert(copied ne parsed)
    assertEquals(copied.origin.dialectOpt, Some(dialects.Scala213))
    assertEquals(copied.pos, Position.None)
    val set = parsed.toBuilder.origin(Origin.DialectOnly(dialects.Scala3)).result()
    assertEquals(set.origin.dialectOpt, Some(dialects.Scala3))
    assertEquals(parsed.origin.dialectOpt, Some(dialects.Scala213))
    assertEquals(parsed.pos.start, 4)
  }

  test("toBuilder: the original's comments stay") {
    val source = dialect("/* c */ x // d").parse[Term].get.asInstanceOf[Term.Name]
    val tree = source.toBuilder.result()
    assert(tree ne source)
    assertTree(tree)(source)
    assertEquals(tree.begComment.map(_.values.head.parts.head.value), Some("/* c */"))
    assertEquals(tree.endComment.map(_.values.head.parts.head.value), Some("// d"))
  }

  test("newBuilder: a top-level leaf") {
    val tree = Self.newBuilder(tname("self"), None).result()
    assertTree(tree)(Self(tname("self"), None))
  }

  test("toBuilder: one field changed") {
    val source = dialect("if (a) b else c").parse[Term].get.asInstanceOf[Term.If]
    val tree = source.toBuilder.elsep(int(3)).result()
    assert(tree ne source)
    assertTree(tree)(Term.If(tname("a"), tname("b"), int(3)))
    assert(tree.cond ne source.cond)
    assert(tree.cond.parent.contains(tree))
    assert(tree.elsep.parent.contains(tree))
    assertEquals(tree.pos, Position.None)
    assertEquals(tree.cond.pos.start, 4)
    assertEquals(tree.syntax, "if (a) b else 3")
    assertTree(source)(Term.If(tname("a"), tname("b"), tname("c")))
    assert(source.cond.parent.contains(source))
  }

  test("toBuilder: comments") {
    val source = tname("x")
    val tree = source.toBuilder.endComment(comments("// e")).result()
    assert(tree ne source)
    assertStruct(tree)("Term.Name(\"x\") // e")
    assertEquals(source.endComment, None)
  }

  test("result: a second call") {
    val builder = Term.Name.newBuilder("x")
    val tree = builder.result()
    assert(builder.result() eq tree)
  }

  test("toBuilder: a quasi")(
    intercept[UnsupportedOperationException](Term.Name.Quasi(0, tname("x")).toBuilder),
  )

}
