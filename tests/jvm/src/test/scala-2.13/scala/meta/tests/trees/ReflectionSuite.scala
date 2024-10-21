package scala.meta.tests
package trees

import org.scalameta.internal.ScalaCompat.EOL

import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

class ReflectionSuite extends TreeSuiteBase {
  object TreeReflection
      extends {
        val u: ru.type = ru
        val mirror: u.Mirror = u.runtimeMirror(classOf[scala.meta.Tree].getClassLoader)
      }
      with scala.meta.internal.trees.Reflection
  import TreeReflection._

  // NOTE: These counts are important because our TreeReflection infrastructure is quite fragile.
  // Therefore we do need additional safeguards in place to prevent silent failures.
  // I understand that it's inconvenient to update these numbers every time something changes,
  // but please deal with that (or come up with a more effective way of testing TreeReflection)
  test("root") {
    val sym = symbolOf[scala.meta.Tree]
    assert(sym.isRoot)
    val root = sym.asRoot
    assertEquals((root.allBranches.length, root.allLeafs.length), (63, 443))
  }

  test("If") {
    val iff = symbolOf[scala.meta.Term.If].asLeaf
    assertEquals(
      iff.fields.map(_.toString),
      List(
        "field Term.If.cond: scala.meta.Term",
        "field Term.If.thenp: scala.meta.Term",
        "field Term.If.elsep: scala.meta.Term",
        "field Term.If.mods: List[scala.meta.Mod]"
      )
    )
    assertEquals(
      iff.allFields.map(_.toString),
      List(
        "field Term.If.origin: scala.meta.trees.Origin",
        "field Term.If.cond: scala.meta.Term",
        "field Term.If.thenp: scala.meta.Term",
        "field Term.If.elsep: scala.meta.Term",
        "field Term.If.mods: List[scala.meta.Mod]"
      )
    )
  }

  test("Term.Name") {
    val iff = symbolOf[scala.meta.Term.Name].asLeaf
    assertEquals(
      iff.fields.map(_.toString),
      List("field Term.Name.value: String @org.scalameta.invariants.nonEmpty")
    )
    assertEquals(
      iff.allFields.map(_.toString),
      List(
        "field Term.Name.origin: scala.meta.trees.Origin",
        "field Term.Name.value: String @org.scalameta.invariants.nonEmpty"
      )
    )
  }

  test("allFields") {
    val allRelevantFields = symbolOf[scala.meta.Tree].asRoot.allLeafs
      .filter(!_.sym.fullName.endsWith(".Quasi")).flatMap(_.fields)
    val duplicateRelevantFieldTpes = allRelevantFields.map(_.tpe)
      .map { case AnnotatedType(_, tpe) => tpe; case tpe => tpe }
    // NOTE: we can't just do `duplicateRelevantFieldTpes.distinct`, because that doesn't account for `=:=`
    val distinctRelevantFieldTpes = ListBuffer[Type]()
    duplicateRelevantFieldTpes.foreach(tpe =>
      if (!distinctRelevantFieldTpes.exists(_ =:= tpe)) distinctRelevantFieldTpes += tpe
    )
    val obtained = distinctRelevantFieldTpes.sortBy(_.toString).mkString(EOL)
    assertNoDiff(
      obtained,
      """|
         |Boolean
         |Byte
         |Char
         |Int
         |List[meta.Term.ParamClause]
         |List[scala.meta.Case]
         |List[scala.meta.Enumerator]
         |List[scala.meta.Importee]
         |List[scala.meta.Importer]
         |List[scala.meta.Init]
         |List[scala.meta.Lit]
         |List[scala.meta.Member.ParamClauseGroup]
         |List[scala.meta.Mod.Annot]
         |List[scala.meta.Mod]
         |List[scala.meta.Pat]
         |List[scala.meta.Source]
         |List[scala.meta.Stat.TypeDef]
         |List[scala.meta.Stat]
         |List[scala.meta.Term.Name]
         |List[scala.meta.Term.Param]
         |List[scala.meta.Term]
         |List[scala.meta.Type.Param]
         |List[scala.meta.TypeCase]
         |List[scala.meta.Type]
         |Long
         |Option[scala.meta.Member.ParamClauseGroup]
         |Option[scala.meta.Mod.ArgsType]
         |Option[scala.meta.Mod.ParamsType]
         |Option[scala.meta.Mod.Variant]
         |Option[scala.meta.Self]
         |Option[scala.meta.Stat.Block]
         |Option[scala.meta.Term.CasesBlock]
         |Option[scala.meta.Term]
         |Option[scala.meta.Type]
         |Seq[scala.meta.Term.ArgClause]
         |Seq[scala.meta.Term.ParamClause]
         |Short
         |String
         |Symbol
         |meta.Type.ParamClause
         |scala.meta.Ctor.Block
         |scala.meta.Ctor.Primary
         |scala.meta.Init
         |scala.meta.Name
         |scala.meta.Pat
         |scala.meta.Pat.ArgClause
         |scala.meta.Pkg.Body
         |scala.meta.Ref
         |scala.meta.Stat
         |scala.meta.Stat.Block
         |scala.meta.Template
         |scala.meta.Template.Body
         |scala.meta.Term
         |scala.meta.Term.ArgClause
         |scala.meta.Term.CasesBlock
         |scala.meta.Term.EnumeratorsBlock
         |scala.meta.Term.Name
         |scala.meta.Term.ParamClause
         |scala.meta.Term.Ref
         |scala.meta.Type
         |scala.meta.Type.ArgClause
         |scala.meta.Type.Bounds
         |scala.meta.Type.CasesBlock
         |scala.meta.Type.FuncParamClause
         |scala.meta.Type.Name
         |""".stripMargin.lf2nl
    )
  }
}
