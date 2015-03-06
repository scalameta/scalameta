import scala.meta.Tree
import scala.meta.internal.ast.Term.{If, Name}
import scala.compat.Platform.EOL
import scala.collection.mutable.ListBuffer

class ReflectionSuite extends AdtSuite {
  import AdtReflection._

  // NOTE: These counts are important because our AdtReflection infrastructure for @ast classes is very fragile.
  // Therefore we do need additional safeguards in place to prevent silent failures.
  // I understand that it's inconvenient to update these numbers every time something changes,
  // but please deal with that (or come up with a more effective way of testing AdtReflection)
  test("root") {
    assert(symbolOf[Tree].isRoot)
    assert(symbolOf[Tree].asRoot.allBranches.length === 64)
    assert(symbolOf[Tree].asRoot.allLeafs.length === 131)
  }

  test("If") {
    val iff = symbolOf[If].asLeaf
    val List(f1, f2, f3) = iff.fields
    assert(f1.toString === "field cond: scala.meta.internal.ast.Term")
    assert(f2.toString === "field thenp: scala.meta.internal.ast.Term")
    assert(f3.toString === "field elsep: scala.meta.internal.ast.Term")
    val List(a1, a2, a3) = iff.allFields
    assert(a1.toString === "field cond: scala.meta.internal.ast.Term")
    assert(a2.toString === "field thenp: scala.meta.internal.ast.Term")
    assert(a3.toString === "field elsep: scala.meta.internal.ast.Term")
  }

  test("Term.Name") {
    val iff = symbolOf[Name].asLeaf
    val List(f1) = iff.fields
    assert(f1.toString === "field value: String @org.scalameta.invariants.nonEmpty")
    val List(a1, a2, a3) = iff.allFields
    assert(a1.toString === "field value: String @org.scalameta.invariants.nonEmpty")
    assert(a2.toString === "field denot: scala.meta.internal.hygiene.Denotation (auxiliary)")
    assert(a3.toString === "field sigma: scala.meta.internal.hygiene.Sigma (auxiliary)")
  }

  // NOTE: Similarly to the `root` test, this test is annoying, but also important.
  // We need to keep an eye on possible field types to make sure that they are unquotable.
  test("allFields") {
    import scala.reflect.runtime.universe.{AnnotatedType, AnnotatedTypeTag}
    val allFields = symbolOf[Tree].asRoot.allLeafs.flatMap(_.allFields)
    val duplicateFieldTpes = allFields.map(_.tpe).map{ case AnnotatedType(_, tpe) => tpe; case tpe => tpe }
    // NOTE: we can't just do `duplicateFieldTpes.distinct`, because that doesn't account for `=:=`
    val distinctFieldTpes = ListBuffer[scala.reflect.runtime.universe.Type]()
    duplicateFieldTpes.foreach(tpe => if (!distinctFieldTpes.exists(_ =:= tpe)) distinctFieldTpes += tpe)
    val allFieldTpes = distinctFieldTpes.toList
    assert(allFieldTpes.sortBy(_.toString).mkString(EOL) === """
      |String
      |scala.Boolean
      |scala.Byte
      |scala.Char
      |scala.Double
      |scala.Float
      |scala.Int
      |scala.Long
      |scala.Option[scala.collection.immutable.Seq[scala.meta.internal.ast.Stat]]
      |scala.Option[scala.meta.internal.ast.Term]
      |scala.Option[scala.meta.internal.ast.Type.Arg]
      |scala.Option[scala.meta.internal.ast.Type]
      |scala.Short
      |scala.Symbol
      |scala.collection.immutable.Seq[scala.collection.immutable.Seq[scala.meta.internal.ast.Term.Arg]]
      |scala.collection.immutable.Seq[scala.collection.immutable.Seq[scala.meta.internal.ast.Term.Param]]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Case]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Enumerator]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Import.Clause]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Import.Selector]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Lit.String]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Mod.Annot]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Mod]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Pat.Arg]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Pat.Type]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Pat.Var.Term]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Pat]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Stat]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Term.Arg]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Term.Param]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Term]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Type.Arg]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Type.Param]
      |scala.collection.immutable.Seq[scala.meta.internal.ast.Type]
      |scala.meta.internal.ast.Ctor.Name
      |scala.meta.internal.ast.Ctor.Primary
      |scala.meta.internal.ast.Name.Indeterminate
      |scala.meta.internal.ast.Name.Qualifier
      |scala.meta.internal.ast.Pat
      |scala.meta.internal.ast.Pat.Arg
      |scala.meta.internal.ast.Pat.Type
      |scala.meta.internal.ast.Pat.Var.Term
      |scala.meta.internal.ast.Template
      |scala.meta.internal.ast.Term
      |scala.meta.internal.ast.Term.Block
      |scala.meta.internal.ast.Term.Name
      |scala.meta.internal.ast.Term.Param
      |scala.meta.internal.ast.Term.Param.Name
      |scala.meta.internal.ast.Term.Ref
      |scala.meta.internal.ast.Type
      |scala.meta.internal.ast.Type.Bounds
      |scala.meta.internal.ast.Type.Name
      |scala.meta.internal.ast.Type.Param.Name
      |scala.meta.internal.hygiene.Denotation
      |scala.meta.internal.hygiene.Sigma
    """.trim.stripMargin)
  }
}