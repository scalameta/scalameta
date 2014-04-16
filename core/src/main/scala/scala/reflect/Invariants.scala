package scala.reflect

// TODO: write tests for all the invariants

object Invariants {
  // TODO: implement this with a macro that fetches and checks an invariant that corresponds to T if any
  def check[T](artifact: T): Unit = ???

  def Interpolate(tree: Tree.Term.Interpolate): Unit = {
    // TODO: also check that prefix is alphanumeric
    require(tree.parts.nonEmpty, "Term.Interpolate's parts must not be empty")
    require(tree.parts.length == tree.args.length + 1, "Term.Interpolate args' size must be one less than its parts")
  }

  def Match(tree: Tree.Term.Match): Unit = {
    require(tree.cases.nonEmpty, "Term.Match must contain at least one case")
  }

  def Function(tree: Tree.Term.Function): Unit = {
    require(tree.params.length == 1 || tree.params.forall(!_.annots.contains(Tree.Annot.Implicit)),
            "function can only have one implicit param")
  }

  def PartialFunction(tree: Tree.Term.PartialFunction): Unit = {
    require(tree.cases.nonEmpty, "Term.PartialFunction must contain at least one case")
  }

  def Extractor(tree: Tree.Pat.Extractor): Unit = {
    require(tree.ref.isStableId, "extractor pattern ref must be a stable id")
  }

  def For(tree: Tree.Term.For): Unit = {
    // TODO: invariant: first element must be generator, at least one element
  }

  def Typed(tree: Tree.Pat.Typed): Unit = {
    require(tree.lhs.isInstanceOf[Tree.Pat.Wildcard] || tree.lhs.isInstanceOf[Tree.Term.Ident],
            "Pat.Type's lhs must be either Pat.Wildcard or Term.Ident")
  }

  def Select(tree: Tree.Type.Select): Unit = {
    require(tree.qual.isPath, "Type.Select's qual must be a path")
  }

  def Singleton(tree: Tree.Type.Singleton): Unit = {
    require(tree.ref.isPath, "Type.Singleton's ref must be a path")
  }

  def Package(tree: Tree.Defn.Package): Unit = {
    require(tree.ref.isQualId, "Defn.Package's ref must be a qualifier id")
  }

  def Clause(tree: Tree.Import.Clause): Unit = {
    require(tree.ref.isStableId, "Import.Clause's ref must be a stable id")
  }

  def Template(tree: Tree.Template): Unit = {
    require(tree.parents.length == 0 || tree.parents.tail.forall(_.argss.isEmpty),
            "only first Template parent may have value parameters")
  }
}