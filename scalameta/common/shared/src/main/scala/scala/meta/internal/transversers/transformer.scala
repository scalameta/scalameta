package scala.meta
package internal
package transversers

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.meta.internal.trees.{Metadata => AstMetadata}

class transformer extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TransformerMacros.impl
}

class TransformerMacros(val c: Context) extends TransverserMacros {
  import c.universe._

  def transformField(treeName: TermName)(f: Field): ValDef = {
    def treeTransformer(input: Tree, tpe: Type): Tree = {
      val from = c.freshName(TermName("from"))
      val to = c.freshName(TermName("to"))
      q"""
          val $from = $input
          val $to = apply($from)
          $to match {
            case $to: ${hygienicRef(tpe.typeSymbol)} =>
              if ($from ne $to) same = false
              $to
            case $to =>
              this.fail(${f.owner.prefix + "." + f.name}, $from, $to)
          }
        """
    }
    def optionTransformer(input: Tree, tpe: Type, nested: (Tree, Type) => Tree): Tree = {
      val fromopt = c.freshName(TermName("fromopt"))
      val from = c.freshName(TermName("from"))
      val to = c.freshName(TermName("to"))
      q"""
          val $fromopt = $input
          $fromopt match {
            case $SomeModule($from) =>
              val $to = ${nested(q"$from", tpe.typeArgs.head)}
              if ($from eq $to) $fromopt
              else $SomeModule($to)
            case $NoneModule =>
              $NoneModule
          }
        """
    }
    def listTransformer(input: Tree, tpe: Type, nested: (Tree, Type) => Tree): Tree = {
      val fromlist = c.freshName(TermName("fromlist"))
      val from = c.freshName(TermName("from"))
      val to = c.freshName(TermName("to"))
      q"""
          val $fromlist = $input
          var samelist = true
          val tolist = $ListBufferModule[${tpe.typeArgs.head}]()
          val it = $fromlist.iterator
          while (it.hasNext) {
            val $from = it.next
            val $to = ${nested(q"$from", tpe.typeArgs.head)}
            if ($from ne $to) samelist = false
            tolist += $to
          }
          if (samelist) $fromlist
          else tolist.toList
        """
    }
    val fname = q"$treeName.${f.name}"
    val rhs = f.tpe match {
      case tpe @ TreeTpe() =>
        treeTransformer(fname, tpe)
      case tpe @ OptionTreeTpe(_) =>
        optionTransformer(fname, tpe, treeTransformer)
      case tpe @ ListTreeTpe(_) =>
        listTransformer(fname, tpe, treeTransformer)
      case tpe @ OptionListTreeTpe(_) =>
        optionTransformer(fname, tpe, listTransformer(_, _, treeTransformer))
      case tpe @ ListListTreeTpe(_) =>
        listTransformer(fname, tpe, listTransformer(_, _, treeTransformer))
      case _ =>
        fname
    }
    q"val ${TermName(f.name.toString + "1")} = $rhs"
  }

  def leafHandler(l: Leaf, treeName: TermName): Tree = {
    val constructor = hygienicRef(l.sym.companion)
    val relevantFields =
      l.fields.filter(f => !(f.tpe =:= typeOf[Any]) && !(f.tpe =:= typeOf[String]))
    if (relevantFields.isEmpty) return q"$treeName"
    val transformedFields: List[ValDef] = relevantFields.map(transformField(treeName))

    val binaryCompatFields = l.binaryCompatFields

    val binaryCompatGetters = binaryCompatFields.map { field =>
      q"val ${field.name} = $treeName.${field.name}"
    }

    val binaryCompatSetters = binaryCompatFields.map { field =>
      val setter = TermName("set" + field.sym.name.toString.capitalize)
      q"newTree.$setter(${TermName(field.name.toString + "1")})"
    }
    q"""
      var same = true
      ..$binaryCompatGetters
      ..$transformedFields
      ..${binaryCompatFields.map(transformField(treeName))}
      if (same) $treeName
      else {
        val newTree = $constructor(..${transformedFields.map(_.name)})
        ..$binaryCompatSetters
        newTree
      }
    """
  }

  def leafHandlerType(): Tree = TreeClass

  def generatedMethods(): Tree = {
    q"""
      def apply(treeopt: $OptionClass[$TreeClass]): $OptionClass[$TreeClass] = treeopt match {
        case $SomeModule(tree) =>
          val tree1 = apply(tree)
          if (tree eq tree1) treeopt
          else $SomeModule(tree1)
        case $NoneModule =>
          $NoneModule
      }

      def apply(trees: $ListClass[$TreeClass]): $ListClass[$TreeClass] = {
        var same = true
        val buf = $ListBufferModule[$TreeClass]()
        val it = trees.iterator
        while (it.hasNext) {
          val tree = it.next
          val tree1 = apply(tree)
          if (tree ne tree1) same = false
          buf += tree1
        }
        if (same) trees
        else buf.toList
      }

      def apply(treesopt: $OptionClass[$ListClass[$TreeClass]])(implicit hack: $Hack1Class): $OptionClass[$ListClass[$TreeClass]] = treesopt match {
        case $SomeModule(trees) =>
          val trees1 = apply(trees)
          if (trees eq trees1) treesopt
          else $SomeModule(trees1)
        case $NoneModule =>
          $NoneModule
      }

      def apply(treess: $ListClass[$ListClass[$TreeClass]])(implicit hack: $Hack2Class): $ListClass[$ListClass[$TreeClass]] = {
        var same = true
        val buf = $ListBufferModule[$ListClass[$TreeClass]]()
        val it = treess.iterator
        while (it.hasNext) {
          val trees = it.next
          val trees1 = apply(trees)
          if (trees ne trees1) same = false
          buf += trees1
        }
        if (same) treess
        else buf.toList
      }

      private def fail(field: String, from: $TreeClass, to: $TreeClass): $NothingClass = {
        import scala.meta.prettyprinters._
        val errorPrefix = "Invalid transformation of " + field + ": "
        val errorHeader = errorPrefix + from.productPrefix + " -> " + to.productPrefix + ". "
        val errorDetails = "From: " + from.structure + ", to: " + to.structure
        throw new UnsupportedOperationException(errorHeader + errorDetails)
      }
    """
  }
}
