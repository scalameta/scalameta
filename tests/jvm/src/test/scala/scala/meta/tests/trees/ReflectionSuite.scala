package scala.meta.tests
package trees

import org.scalatest._
import scala.compat.Platform.EOL
import scala.collection.mutable.ListBuffer
import scala.meta.testkit.DiffAssertions
import scala.reflect.runtime.universe._
import scala.reflect.runtime.{universe => ru}

class ReflectionSuite extends FunSuite with DiffAssertions {
  object TreeReflection extends {
    val u: ru.type = ru
    val mirror: u.Mirror = u.runtimeMirror(classOf[scala.meta.Tree].getClassLoader)
  } with scala.meta.internal.trees.Reflection
  import TreeReflection._

  // NOTE: These counts are important because our TreeReflection infrastructure is quite fragile.
  // Therefore we do need additional safeguards in place to prevent silent failures.
  // I understand that it's inconvenient to update these numbers every time something changes,
  // but please deal with that (or come up with a more effective way of testing TreeReflection)
  test("root") {
    assert(symbolOf[scala.meta.Tree].isRoot)
    assert(symbolOf[scala.meta.Tree].asRoot.allBranches.length === 19)
    assert(symbolOf[scala.meta.Tree].asRoot.allLeafs.length === 271)
  }

  test("If") {
    val iff = symbolOf[scala.meta.Term.If].asLeaf
    val List(f1, f2, f3) = iff.fields
    assert(f1.toString === "field Term.If.cond: scala.meta.Term")
    assert(f2.toString === "field Term.If.thenp: scala.meta.Term")
    assert(f3.toString === "field Term.If.elsep: scala.meta.Term")
    val List(a1, a2, a3) = iff.allFields
    assert(a1.toString === "field Term.If.cond: scala.meta.Term")
    assert(a2.toString === "field Term.If.thenp: scala.meta.Term")
    assert(a3.toString === "field Term.If.elsep: scala.meta.Term")
  }

  test("Term.Name") {
    val iff = symbolOf[scala.meta.Term.Name].asLeaf
    val List(f1) = iff.fields
    assert(f1.toString === "field Term.Name.value: String @org.scalameta.invariants.nonEmpty")
    val List(a1) = iff.allFields
    assert(a1.toString === "field Term.Name.value: String @org.scalameta.invariants.nonEmpty")
  }

  test("allFields") {
    val allRelevantFields = symbolOf[scala.meta.Tree].asRoot.allLeafs.filter(!_.sym.fullName.endsWith(".Quasi")).flatMap(_.fields)
    val duplicateRelevantFieldTpes = allRelevantFields.map(_.tpe).map{ case AnnotatedType(_, tpe) => tpe; case tpe => tpe }
    // NOTE: we can't just do `duplicateRelevantFieldTpes.distinct`, because that doesn't account for `=:=`
    val distinctRelevantFieldTpes = ListBuffer[Type]()
    duplicateRelevantFieldTpes.foreach(tpe => if (!distinctRelevantFieldTpes.exists(_ =:= tpe)) distinctRelevantFieldTpes += tpe)
    val obtained = distinctRelevantFieldTpes.sortBy(_.toString).mkString(EOL)
    assertNoDiff(
      obtained,
      """
      |Boolean
      |Byte
      |Char
      |Int
      |List[List[scala.meta.Term.Param]]
      |List[List[scala.meta.Term]]
      |List[scala.meta.Case]
      |List[scala.meta.Enumerator]
      |List[scala.meta.Importee]
      |List[scala.meta.Importer]
      |List[scala.meta.Init]
      |List[scala.meta.Lit]
      |List[scala.meta.Mod.Annot]
      |List[scala.meta.Mod]
      |List[scala.meta.Pat]
      |List[scala.meta.Stat]
      |List[scala.meta.Term.Param]
      |List[scala.meta.Term]
      |List[scala.meta.Type.Param]
      |List[scala.meta.Type]
      |Long
      |Option[scala.meta.Term]
      |Option[scala.meta.Type]
      |Short
      |String
      |Symbol
      |scala.meta.Ctor.Primary
      |scala.meta.Init
      |scala.meta.Name
      |scala.meta.Pat
      |scala.meta.Ref
      |scala.meta.Self
      |scala.meta.Template
      |scala.meta.Term
      |scala.meta.Term.Name
      |scala.meta.Term.Ref
      |scala.meta.Type
      |scala.meta.Type.Bounds
      |scala.meta.Type.Name
    """.trim.stripMargin.split('\n').mkString(EOL)
    )
  }
}
