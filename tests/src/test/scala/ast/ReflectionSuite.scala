import scala.compat.Platform.EOL
import scala.collection.mutable.ListBuffer
import scala.reflect.runtime.universe._

class ReflectionSuite extends AstSuite {
  import AstReflection._

  // NOTE: These counts are important because our AstReflection infrastructure is very fragile.
  // Therefore we do need additional safeguards in place to prevent silent failures.
  // I understand that it's inconvenient to update these numbers every time something changes,
  // but please deal with that (or come up with a more effective way of testing AstReflection)
  test("root") {
    assert(symbolOf[scala.meta.Tree].isRoot)
    assert(symbolOf[scala.meta.Tree].asRoot.allBranches.length === 69)
    assert(symbolOf[scala.meta.Tree].asRoot.allLeafs.length === 329)
  }

  test("If") {
    val iff = symbolOf[scala.meta.internal.ast.Term.If].asLeaf
    val List(f1, f2, f3) = iff.fields
    assert(f1.toString === "field Term.If.cond: scala.meta.internal.ast.Term")
    assert(f2.toString === "field Term.If.thenp: scala.meta.internal.ast.Term")
    assert(f3.toString === "field Term.If.elsep: scala.meta.internal.ast.Term")
    val List(a1, a2, a3) = iff.allFields
    assert(a1.toString === "field Term.If.cond: scala.meta.internal.ast.Term")
    assert(a2.toString === "field Term.If.thenp: scala.meta.internal.ast.Term")
    assert(a3.toString === "field Term.If.elsep: scala.meta.internal.ast.Term")
  }

  test("Term.Name") {
    val iff = symbolOf[scala.meta.internal.ast.Term.Name].asLeaf
    val List(f1) = iff.fields
    assert(f1.toString === "field Term.Name.value: String @org.scalameta.invariants.nonEmpty")
    val List(a1, a2, a3) = iff.allFields
    assert(a1.toString === "field Term.Name.value: String @org.scalameta.invariants.nonEmpty")
    assert(a2.toString === "field Term.Name.denot: scala.meta.internal.hygiene.Denotation (auxiliary)")
    assert(a3.toString === "field Term.Name.sigma: scala.meta.internal.hygiene.Sigma (auxiliary)")
  }

  test("all.publish") {
    val all = symbolOf[scala.meta.Tree].asRoot.all.filter(!_.sym.fullName.endsWith(".Quasi")).sortBy(_.sym.fullName)
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
      |scala.meta.Pat.Var                               => scala.meta.Pat.Var
      |scala.meta.Pat.Var.Term                          => scala.meta.Pat.Var.Term
      |scala.meta.Pat.Var.Type                          => scala.meta.Pat.Var.Type
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
      |scala.meta.internal.ast.Pat.Var                  => scala.meta.Pat.Var
      |scala.meta.internal.ast.Pat.Var.Term             => scala.meta.Pat.Var.Term
      |scala.meta.internal.ast.Pat.Var.Type             => scala.meta.Pat.Var.Type
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
    """.trim.stripMargin)
  }

  test("allFields.publish") {
    val allRelevantFields = symbolOf[scala.meta.Tree].asRoot.allLeafs.filter(!_.sym.fullName.endsWith(".Quasi")).flatMap(_.fields)
    val duplicateRelevantFieldTpes = allRelevantFields.map(_.tpe).map{ case AnnotatedType(_, tpe) => tpe; case tpe => tpe }
    // NOTE: we can't just do `duplicateRelevantFieldTpes.distinct`, because that doesn't account for `=:=`
    val distinctRelevantFieldTpes = ListBuffer[Type]()
    duplicateRelevantFieldTpes.foreach(tpe => if (!distinctRelevantFieldTpes.exists(_ =:= tpe)) distinctRelevantFieldTpes += tpe)
    assert(distinctRelevantFieldTpes.sortBy(_.toString).mkString(EOL) === """
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
      |scala.meta.internal.ast.Ctor.Primary
      |scala.meta.internal.ast.Ctor.Ref.Name
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

  test("allFields.unquote") {
    implicit class XtensionType(tpe: Type) {
      def unwrap: Type = tpe match {
        case AnnotatedType(_, tpe) => tpe.unwrap
        case _ if tpe.typeSymbol == symbolOf[Option[_]] => tpe.typeArgs.head.unwrap
        case _ if tpe.typeSymbol == symbolOf[scala.collection.immutable.Seq[_]] => tpe.typeArgs.head.unwrap
        case tpe => tpe
      }
    }
    val all = symbolOf[scala.meta.Tree].asRoot.all.sortBy(_.sym.fullName)
    val allLeafs = symbolOf[scala.meta.Tree].asRoot.allLeafs.sortBy(_.sym.fullName)
    val allRelevantLeafs = allLeafs.filter(!_.sym.fullName.endsWith(".Quasi"))
    val allRelevantFields = allRelevantLeafs.flatMap(_.fields)
    val distinctRelevantFieldTpes = ListBuffer[Type]()
    allRelevantFields.map(_.tpe.unwrap).foreach(tpe => if (!distinctRelevantFieldTpes.exists(_ =:= tpe)) distinctRelevantFieldTpes += tpe)
    val report = ListBuffer[String]()
    distinctRelevantFieldTpes.map(ftpe => {
      val ambig = allRelevantLeafs.filter(node => (node.tpe.publish <:< ftpe.publish) && !(node.tpe <:< ftpe))
      if (ambig.nonEmpty) {
        report += s"$ftpe -> ${ftpe.publish}"
        allRelevantFields.filter(f => f.tpe.unwrap =:= ftpe).foreach(f => report += f.toString)
        report += ""
      }
    })
    // NOTE: if something in this report changes, you must update requirements in @ast impl (see "step 10: finish codegen for Quasi")
    assert(report.mkString(EOL) === """
      |scala.meta.internal.ast.Term.Block -> scala.meta.Term with scala.meta.Scope
      |field Case.body: scala.meta.internal.ast.Term.Block
      |
      |scala.meta.internal.ast.Type.Bounds -> scala.meta.Tree
      |field Decl.Type.bounds: scala.meta.internal.ast.Type.Bounds
      |field Type.Param.typeBounds: scala.meta.internal.ast.Type.Bounds
      |field Type.Placeholder.bounds: scala.meta.internal.ast.Type.Bounds
      |
      |scala.meta.internal.ast.Ctor.Primary -> scala.meta.Member.Term
      |field Defn.Class.ctor: scala.meta.internal.ast.Ctor.Primary
      |field Defn.Object.ctor: scala.meta.internal.ast.Ctor.Primary
      |field Defn.Trait.ctor: scala.meta.internal.ast.Ctor.Primary
      |field Pkg.Object.ctor: scala.meta.internal.ast.Ctor.Primary
      |
      |scala.meta.internal.ast.Import.Clause -> scala.meta.Tree
      |field Import.clauses: scala.collection.immutable.Seq[scala.meta.internal.ast.Import.Clause] @org.scalameta.invariants.nonEmpty
      |
      |scala.meta.internal.ast.Lit.String -> scala.meta.Lit
      |field Pat.Interpolate.parts: scala.collection.immutable.Seq[scala.meta.internal.ast.Lit.String] @org.scalameta.invariants.nonEmpty
      |field Term.Interpolate.parts: scala.collection.immutable.Seq[scala.meta.internal.ast.Lit.String] @org.scalameta.invariants.nonEmpty
      |
      |scala.meta.internal.ast.Mod.Annot -> scala.meta.Mod
      |field Pat.Type.Annotate.annots: scala.collection.immutable.Seq[scala.meta.internal.ast.Mod.Annot] @org.scalameta.invariants.nonEmpty
      |field Term.Annotate.annots: scala.collection.immutable.Seq[scala.meta.internal.ast.Mod.Annot] @org.scalameta.invariants.nonEmpty
      |field Type.Annotate.annots: scala.collection.immutable.Seq[scala.meta.internal.ast.Mod.Annot] @org.scalameta.invariants.nonEmpty
      |
    """.trim.stripMargin)
  }
}