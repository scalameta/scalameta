import scala.meta.Tree
import scala.meta.internal.ast.Term.{If, Name}
import scala.compat.Platform.EOL
import scala.collection.mutable.ListBuffer

class ReflectionSuite extends AstSuite {
  import AstReflection._

  // NOTE: These counts are important because our AstReflection infrastructure is very fragile.
  // Therefore we do need additional safeguards in place to prevent silent failures.
  // I understand that it's inconvenient to update these numbers every time something changes,
  // but please deal with that (or come up with a more effective way of testing AstReflection)
  test("root") {
    assert(symbolOf[Tree].isRoot)
    assert(symbolOf[Tree].asRoot.allBranches.length === 65)
    assert(symbolOf[Tree].asRoot.allLeafs.length === 132)
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
    val irrelevant = Set("scala.meta.internal.ast.Ellipsis", "scala.meta.internal.ast.Unquote")
    val allFields = symbolOf[Tree].asRoot.allLeafs.filter(leaf => !irrelevant(leaf.sym.fullName)).flatMap(_.fields)
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
    """.trim.stripMargin)
  }

  // NOTE: Similarly to the `root` and `allFields` test, this test is annoying, but also important.
  test("publish") {
    val all = symbolOf[Tree].asRoot.all.sortBy(_.sym.fullName)
    val published = all.map(adt => (adt.tpe, adt.tpe.publish))
    val columnSize = published.map(_._1.typeSymbol.fullName.length).max
    assert(published.map(kvp => s"%-${columnSize}s => %s".format(kvp._1, kvp._2)).mkString(EOL) === """
      |scala.meta.Case                                  => scala.meta.Case
      |scala.meta.Ctor.Name                             => scala.meta.Ctor.Name
      |scala.meta.Ctor.Ref                              => scala.meta.Ctor.Ref
      |scala.meta.Enumerator                            => scala.meta.Enumerator
      |scala.meta.Importee                              => scala.meta.Importee
      |scala.meta.Lit                                   => scala.meta.Lit
      |scala.meta.Member                                => scala.meta.Member
      |scala.meta.Member.Term                           => scala.meta.Member.Term
      |scala.meta.Member.Type                           => scala.meta.Member.Type
      |scala.meta.Mod                                   => scala.meta.Mod
      |scala.meta.Name                                  => scala.meta.Name
      |scala.meta.Name.Anonymous                        => scala.meta.Name.Anonymous
      |scala.meta.Name.Indeterminate                    => scala.meta.Name.Indeterminate
      |scala.meta.Name.Qualifier                        => scala.meta.Name.Qualifier
      |scala.meta.Pat                                   => scala.meta.Pat
      |scala.meta.Pat.Arg                               => scala.meta.Pat.Arg
      |scala.meta.Pat.Type                              => scala.meta.Pat.Type
      |scala.meta.Pat.Type.Ref                          => scala.meta.Pat.Type.Ref
      |scala.meta.Ref                                   => scala.meta.Ref
      |scala.meta.Scope                                 => scala.meta.Scope
      |scala.meta.Source                                => scala.meta.Source
      |scala.meta.Stat                                  => scala.meta.Stat
      |scala.meta.Template                              => scala.meta.Template
      |scala.meta.Term                                  => scala.meta.Term
      |scala.meta.Term.Arg                              => scala.meta.Term.Arg
      |scala.meta.Term.Name                             => scala.meta.Term.Name
      |scala.meta.Term.Param                            => scala.meta.Term.Param
      |scala.meta.Term.Param.Name                       => scala.meta.Term.Param.Name
      |scala.meta.Term.Ref                              => scala.meta.Term.Ref
      |scala.meta.Tree                                  => scala.meta.Tree
      |scala.meta.Type                                  => scala.meta.Type
      |scala.meta.Type.Arg                              => scala.meta.Type.Arg
      |scala.meta.Type.Name                             => scala.meta.Type.Name
      |scala.meta.Type.Param                            => scala.meta.Type.Param
      |scala.meta.Type.Param.Name                       => scala.meta.Type.Param.Name
      |scala.meta.Type.Ref                              => scala.meta.Type.Ref
      |scala.meta.internal.ast.Case                     => scala.meta.Case
      |scala.meta.internal.ast.Ctor                     => scala.meta.Member.Term
      |scala.meta.internal.ast.Ctor.Primary             => scala.meta.Member.Term
      |scala.meta.internal.ast.Ctor.Ref                 => scala.meta.Ctor.Ref
      |scala.meta.internal.ast.Ctor.Ref.Function        => scala.meta.Ctor.Ref
      |scala.meta.internal.ast.Ctor.Ref.Name            => scala.meta.Ctor.Name
      |scala.meta.internal.ast.Ctor.Ref.Project         => scala.meta.Ctor.Ref
      |scala.meta.internal.ast.Ctor.Ref.Select          => scala.meta.Ctor.Ref
      |scala.meta.internal.ast.Ctor.Secondary           => scala.meta.Member.Term with scala.meta.Stat
      |scala.meta.internal.ast.Decl                     => scala.meta.Stat
      |scala.meta.internal.ast.Decl.Def                 => scala.meta.Member.Term with scala.meta.Stat
      |scala.meta.internal.ast.Decl.Type                => scala.meta.Member.Type with scala.meta.Stat
      |scala.meta.internal.ast.Decl.Val                 => scala.meta.Stat
      |scala.meta.internal.ast.Decl.Var                 => scala.meta.Stat
      |scala.meta.internal.ast.Defn                     => scala.meta.Stat
      |scala.meta.internal.ast.Defn.Class               => scala.meta.Member.Type with scala.meta.Stat
      |scala.meta.internal.ast.Defn.Def                 => scala.meta.Member.Term with scala.meta.Stat
      |scala.meta.internal.ast.Defn.Macro               => scala.meta.Member.Term with scala.meta.Stat
      |scala.meta.internal.ast.Defn.Object              => scala.meta.Member.Term with scala.meta.Stat
      |scala.meta.internal.ast.Defn.Trait               => scala.meta.Member.Type with scala.meta.Stat
      |scala.meta.internal.ast.Defn.Type                => scala.meta.Member.Type with scala.meta.Stat
      |scala.meta.internal.ast.Defn.Val                 => scala.meta.Stat
      |scala.meta.internal.ast.Defn.Var                 => scala.meta.Stat
      |scala.meta.internal.ast.Ellipsis                 => Nothing
      |scala.meta.internal.ast.Enumerator               => scala.meta.Enumerator
      |scala.meta.internal.ast.Enumerator.Generator     => scala.meta.Enumerator
      |scala.meta.internal.ast.Enumerator.Guard         => scala.meta.Enumerator
      |scala.meta.internal.ast.Enumerator.Val           => scala.meta.Enumerator
      |scala.meta.internal.ast.Import                   => scala.meta.Stat
      |scala.meta.internal.ast.Import.Clause            => scala.meta.Tree
      |scala.meta.internal.ast.Import.Selector          => scala.meta.Importee
      |scala.meta.internal.ast.Import.Selector.Name     => scala.meta.Importee
      |scala.meta.internal.ast.Import.Selector.Rename   => scala.meta.Importee
      |scala.meta.internal.ast.Import.Selector.Unimport => scala.meta.Importee
      |scala.meta.internal.ast.Import.Selector.Wildcard => scala.meta.Importee
      |scala.meta.internal.ast.Lit                      => scala.meta.Lit
      |scala.meta.internal.ast.Lit.Bool                 => scala.meta.Lit
      |scala.meta.internal.ast.Lit.Byte                 => scala.meta.Lit
      |scala.meta.internal.ast.Lit.Char                 => scala.meta.Lit
      |scala.meta.internal.ast.Lit.Double               => scala.meta.Lit
      |scala.meta.internal.ast.Lit.Float                => scala.meta.Lit
      |scala.meta.internal.ast.Lit.Int                  => scala.meta.Lit
      |scala.meta.internal.ast.Lit.Long                 => scala.meta.Lit
      |scala.meta.internal.ast.Lit.Null                 => scala.meta.Lit
      |scala.meta.internal.ast.Lit.Short                => scala.meta.Lit
      |scala.meta.internal.ast.Lit.String               => scala.meta.Lit
      |scala.meta.internal.ast.Lit.Symbol               => scala.meta.Lit
      |scala.meta.internal.ast.Lit.Unit                 => scala.meta.Lit
      |scala.meta.internal.ast.Member                   => scala.meta.Member
      |scala.meta.internal.ast.Member.Term              => scala.meta.Member.Term
      |scala.meta.internal.ast.Member.Type              => scala.meta.Member.Type
      |scala.meta.internal.ast.Mod                      => scala.meta.Mod
      |scala.meta.internal.ast.Mod.Abstract             => scala.meta.Mod
      |scala.meta.internal.ast.Mod.Annot                => scala.meta.Mod
      |scala.meta.internal.ast.Mod.Case                 => scala.meta.Mod
      |scala.meta.internal.ast.Mod.Contravariant        => scala.meta.Mod
      |scala.meta.internal.ast.Mod.Covariant            => scala.meta.Mod
      |scala.meta.internal.ast.Mod.Ffi                  => scala.meta.Mod
      |scala.meta.internal.ast.Mod.Final                => scala.meta.Mod
      |scala.meta.internal.ast.Mod.Implicit             => scala.meta.Mod
      |scala.meta.internal.ast.Mod.Lazy                 => scala.meta.Mod
      |scala.meta.internal.ast.Mod.Override             => scala.meta.Mod
      |scala.meta.internal.ast.Mod.Private              => scala.meta.Mod
      |scala.meta.internal.ast.Mod.Protected            => scala.meta.Mod
      |scala.meta.internal.ast.Mod.Sealed               => scala.meta.Mod
      |scala.meta.internal.ast.Mod.ValParam             => scala.meta.Mod
      |scala.meta.internal.ast.Mod.VarParam             => scala.meta.Mod
      |scala.meta.internal.ast.Name                     => scala.meta.Name
      |scala.meta.internal.ast.Name.Anonymous           => scala.meta.Name.Anonymous
      |scala.meta.internal.ast.Name.Indeterminate       => scala.meta.Name.Indeterminate
      |scala.meta.internal.ast.Name.Qualifier           => scala.meta.Name.Qualifier
      |scala.meta.internal.ast.Pat                      => scala.meta.Pat
      |scala.meta.internal.ast.Pat.Alternative          => scala.meta.Pat
      |scala.meta.internal.ast.Pat.Arg                  => scala.meta.Pat.Arg
      |scala.meta.internal.ast.Pat.Arg.SeqWildcard      => scala.meta.Pat.Arg
      |scala.meta.internal.ast.Pat.Bind                 => scala.meta.Pat
      |scala.meta.internal.ast.Pat.Extract              => scala.meta.Pat
      |scala.meta.internal.ast.Pat.ExtractInfix         => scala.meta.Pat
      |scala.meta.internal.ast.Pat.Interpolate          => scala.meta.Pat
      |scala.meta.internal.ast.Pat.Tuple                => scala.meta.Pat
      |scala.meta.internal.ast.Pat.Type                 => scala.meta.Pat.Type
      |scala.meta.internal.ast.Pat.Type.Annotate        => scala.meta.Pat.Type
      |scala.meta.internal.ast.Pat.Type.Apply           => scala.meta.Pat.Type
      |scala.meta.internal.ast.Pat.Type.ApplyInfix      => scala.meta.Pat.Type
      |scala.meta.internal.ast.Pat.Type.Compound        => scala.meta.Pat.Type
      |scala.meta.internal.ast.Pat.Type.Existential     => scala.meta.Pat.Type
      |scala.meta.internal.ast.Pat.Type.Function        => scala.meta.Pat.Type
      |scala.meta.internal.ast.Pat.Type.Project         => scala.meta.Pat.Type.Ref
      |scala.meta.internal.ast.Pat.Type.Ref             => scala.meta.Pat.Type.Ref
      |scala.meta.internal.ast.Pat.Type.Tuple           => scala.meta.Pat.Type
      |scala.meta.internal.ast.Pat.Type.Wildcard        => scala.meta.Pat.Type
      |scala.meta.internal.ast.Pat.Typed                => scala.meta.Pat
      |scala.meta.internal.ast.Pat.Var                  => scala.meta.Tree
      |scala.meta.internal.ast.Pat.Var.Term             => scala.meta.Member.Term with scala.meta.Pat
      |scala.meta.internal.ast.Pat.Var.Type             => scala.meta.Member.Type with scala.meta.Pat.Type
      |scala.meta.internal.ast.Pat.Wildcard             => scala.meta.Pat
      |scala.meta.internal.ast.Pkg                      => scala.meta.Member.Term with scala.meta.Stat
      |scala.meta.internal.ast.Pkg.Object               => scala.meta.Member.Term with scala.meta.Stat
      |scala.meta.internal.ast.Ref                      => scala.meta.Ref
      |scala.meta.internal.ast.Scope                    => scala.meta.Scope
      |scala.meta.internal.ast.Source                   => scala.meta.Source
      |scala.meta.internal.ast.Stat                     => scala.meta.Stat
      |scala.meta.internal.ast.Template                 => scala.meta.Template
      |scala.meta.internal.ast.Term                     => scala.meta.Term
      |scala.meta.internal.ast.Term.Annotate            => scala.meta.Term
      |scala.meta.internal.ast.Term.Apply               => scala.meta.Term
      |scala.meta.internal.ast.Term.ApplyInfix          => scala.meta.Term
      |scala.meta.internal.ast.Term.ApplyType           => scala.meta.Term
      |scala.meta.internal.ast.Term.ApplyUnary          => scala.meta.Term
      |scala.meta.internal.ast.Term.Arg                 => scala.meta.Term.Arg
      |scala.meta.internal.ast.Term.Arg.Named           => scala.meta.Term.Arg
      |scala.meta.internal.ast.Term.Arg.Repeated        => scala.meta.Term.Arg
      |scala.meta.internal.ast.Term.Ascribe             => scala.meta.Term
      |scala.meta.internal.ast.Term.Assign              => scala.meta.Term
      |scala.meta.internal.ast.Term.Block               => scala.meta.Term with scala.meta.Scope
      |scala.meta.internal.ast.Term.Do                  => scala.meta.Term
      |scala.meta.internal.ast.Term.Eta                 => scala.meta.Term
      |scala.meta.internal.ast.Term.For                 => scala.meta.Term with scala.meta.Scope
      |scala.meta.internal.ast.Term.ForYield            => scala.meta.Term with scala.meta.Scope
      |scala.meta.internal.ast.Term.Function            => scala.meta.Term with scala.meta.Scope
      |scala.meta.internal.ast.Term.If                  => scala.meta.Term
      |scala.meta.internal.ast.Term.Interpolate         => scala.meta.Term
      |scala.meta.internal.ast.Term.Match               => scala.meta.Term
      |scala.meta.internal.ast.Term.Name                => scala.meta.Term.Name
      |scala.meta.internal.ast.Term.New                 => scala.meta.Term
      |scala.meta.internal.ast.Term.Param               => scala.meta.Term.Param
      |scala.meta.internal.ast.Term.Param.Name          => scala.meta.Term.Param.Name
      |scala.meta.internal.ast.Term.PartialFunction     => scala.meta.Term
      |scala.meta.internal.ast.Term.Placeholder         => scala.meta.Term
      |scala.meta.internal.ast.Term.Ref                 => scala.meta.Term.Ref
      |scala.meta.internal.ast.Term.Return              => scala.meta.Term
      |scala.meta.internal.ast.Term.Select              => scala.meta.Term.Ref with scala.meta.Pat
      |scala.meta.internal.ast.Term.Super               => scala.meta.Term.Ref
      |scala.meta.internal.ast.Term.This                => scala.meta.Term.Ref with scala.meta.Name.Qualifier
      |scala.meta.internal.ast.Term.Throw               => scala.meta.Term
      |scala.meta.internal.ast.Term.TryWithCases        => scala.meta.Term
      |scala.meta.internal.ast.Term.TryWithTerm         => scala.meta.Term
      |scala.meta.internal.ast.Term.Tuple               => scala.meta.Term
      |scala.meta.internal.ast.Term.Update              => scala.meta.Term
      |scala.meta.internal.ast.Term.While               => scala.meta.Term
      |scala.meta.internal.ast.Tree                     => scala.meta.Tree
      |scala.meta.internal.ast.Type                     => scala.meta.Type
      |scala.meta.internal.ast.Type.Annotate            => scala.meta.Type
      |scala.meta.internal.ast.Type.Apply               => scala.meta.Type
      |scala.meta.internal.ast.Type.ApplyInfix          => scala.meta.Type
      |scala.meta.internal.ast.Type.Arg                 => scala.meta.Type.Arg
      |scala.meta.internal.ast.Type.Arg.ByName          => scala.meta.Type.Arg
      |scala.meta.internal.ast.Type.Arg.Repeated        => scala.meta.Type.Arg
      |scala.meta.internal.ast.Type.Bounds              => scala.meta.Tree
      |scala.meta.internal.ast.Type.Compound            => scala.meta.Type
      |scala.meta.internal.ast.Type.Existential         => scala.meta.Type
      |scala.meta.internal.ast.Type.Function            => scala.meta.Type
      |scala.meta.internal.ast.Type.Name                => scala.meta.Type.Name
      |scala.meta.internal.ast.Type.Param               => scala.meta.Type.Param
      |scala.meta.internal.ast.Type.Param.Name          => scala.meta.Type.Param.Name
      |scala.meta.internal.ast.Type.Placeholder         => scala.meta.Type with scala.meta.Pat.Type
      |scala.meta.internal.ast.Type.Project             => scala.meta.Type.Ref
      |scala.meta.internal.ast.Type.Ref                 => scala.meta.Type.Ref
      |scala.meta.internal.ast.Type.Select              => scala.meta.Type.Ref with scala.meta.Pat.Type.Ref
      |scala.meta.internal.ast.Type.Singleton           => scala.meta.Type.Ref with scala.meta.Pat.Type.Ref
      |scala.meta.internal.ast.Type.Tuple               => scala.meta.Type
      |scala.meta.internal.ast.Unquote                  => Nothing
    """.trim.stripMargin)
  }
}