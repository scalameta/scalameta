package scala.meta.tests
package ast

import scala.compat.Platform.EOL
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._

class ReflectionSuite extends AstSuite {
  import AstReflection._

  // NOTE: These counts are important because our AstReflection infrastructure is quite fragile.
  // Therefore we do need additional safeguards in place to prevent silent failures.
  // I understand that it's inconvenient to update these numbers every time something changes,
  // but please deal with that (or come up with a more effective way of testing AstReflection)
  test("root") {
    assert(symbolOf[scala.meta.Tree].isRoot)
    assert(symbolOf[scala.meta.Tree].asRoot.allBranches.length === 30)
    assert(symbolOf[scala.meta.Tree].asRoot.allLeafs.length === 272)
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
    assert(distinctRelevantFieldTpes.sortBy(_.toString).mkString(EOL) === """
      |String
      |scala.Any
      |scala.Option[scala.collection.immutable.Seq[scala.meta.Stat]]
      |scala.Option[scala.meta.Term]
      |scala.Option[scala.meta.Type.Arg]
      |scala.Option[scala.meta.Type]
      |scala.collection.immutable.Seq[scala.collection.immutable.Seq[scala.meta.Term.Arg]]
      |scala.collection.immutable.Seq[scala.collection.immutable.Seq[scala.meta.Term.Param]]
      |scala.collection.immutable.Seq[scala.meta.Case]
      |scala.collection.immutable.Seq[scala.meta.Ctor.Call]
      |scala.collection.immutable.Seq[scala.meta.Enumerator]
      |scala.collection.immutable.Seq[scala.meta.Importee]
      |scala.collection.immutable.Seq[scala.meta.Importer]
      |scala.collection.immutable.Seq[scala.meta.Lit]
      |scala.collection.immutable.Seq[scala.meta.Mod.Annot]
      |scala.collection.immutable.Seq[scala.meta.Mod]
      |scala.collection.immutable.Seq[scala.meta.Pat.Arg]
      |scala.collection.immutable.Seq[scala.meta.Pat.Type]
      |scala.collection.immutable.Seq[scala.meta.Pat.Var.Term]
      |scala.collection.immutable.Seq[scala.meta.Pat]
      |scala.collection.immutable.Seq[scala.meta.Stat]
      |scala.collection.immutable.Seq[scala.meta.Term.Arg]
      |scala.collection.immutable.Seq[scala.meta.Term.Param]
      |scala.collection.immutable.Seq[scala.meta.Term]
      |scala.collection.immutable.Seq[scala.meta.Type.Arg]
      |scala.collection.immutable.Seq[scala.meta.Type.Param]
      |scala.collection.immutable.Seq[scala.meta.Type]
      |scala.meta.Ctor.Primary
      |scala.meta.Ctor.Ref.Name
      |scala.meta.Name.Indeterminate
      |scala.meta.Name.Qualifier
      |scala.meta.Pat
      |scala.meta.Pat.Arg
      |scala.meta.Pat.Type
      |scala.meta.Pat.Var.Term
      |scala.meta.Template
      |scala.meta.Term
      |scala.meta.Term.Arg
      |scala.meta.Term.Name
      |scala.meta.Term.Param
      |scala.meta.Term.Param.Name
      |scala.meta.Term.Ref
      |scala.meta.Type
      |scala.meta.Type.Bounds
      |scala.meta.Type.Name
      |scala.meta.Type.Param.Name
    """.trim.stripMargin)
  }
}