package scala.reflect

// TODO: write tests for all the invariants

object Invariants {
  // TODO: implement this with a macro that fetches and checks an invariant that corresponds to T if any
  def check[T](x: T): Unit = x match {
    case x: Tree.Term.Interpolate =>
      // TODO: also check that prefix is alphanumeric
      require(x.parts.nonEmpty, "Term.Interpolate's parts must not be empty")
      require(x.parts.length == x.args.length + 1, "Term.Interpolate args' size must be one less than its parts")
    case x: Tree.Term.Match =>
      require(x.cases.nonEmpty, "Term.Match must contain at least one case")
    case x: Tree.Term.Function =>
      require(x.params.length == 1 || x.params.forall(!_.annots.contains(Tree.Annot.Implicit)),
              "function can only have one implicit param")
    case x: Tree.Term.PartialFunction =>
      require(x.cases.nonEmpty, "Term.PartialFunction must contain at least one case")
    case x: Tree.Pat.Extractor =>
      require(x.ref.isStableId, "extractor pattern ref must be a stable id"
    case x: Tree.Term.For =>
      // TODO: invariant: first element must be generator, at least one element
    case x: Tree.Pat.Typed =>
      require(x.lhs.isInstanceOf[Tree.Pat.Wildcard] || x.lhs.isInstanceOf[Tree.Term.Ident],
              "Pat.Type's lhs must be either Pat.Wildcard or Term.Ident")
    case x: Tree.Type.Select =>
      require(x.qual.isPath, "Type.Select's qual must be a path")
    case x: Tree.Type.Singleton =>
      require(x.ref.isPath, "Type.Singleton's ref must be a path")
    case x: Tree.Defn.Package =>
      require(x.ref.isQualId, "Defn.Package's ref must be a qualifier id")
    case x: Tree.Import.Clause =>
      require(x.ref.isStableId, "Import.Clause's ref must be a stable id")
    case x: Tree.Template =>
      require(x.parents.length == 0 || x.parents.tail.forall(_.argss.isEmpty),
              "only first Template parent may have value parameters")
  }
}