package scala.meta
package internal
package transversers

import scala.annotation.StaticAnnotation
import scala.language.experimental.macros
import scala.reflect.macros.whitebox.Context

class transformer extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TransformerMacros.impl
}

class TransformerMacros(val c: Context) extends TransverserMacros {
  import c.universe._

  override def applyModifiers: Modifiers = Modifiers(Flag.PROTECTED)

  def transformField(treeName: TermName)(f: Field): ValDef = {
    def treeTransformer(input: Tree, tpe: Type): Tree = {
      val from = c.freshName(TermName("from"))
      val to = c.freshName(TermName("to"))
      q"""
          val $from = $input
          val $to = transformChild($from)
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
              val $to = ${nested(q"$from", tpe)}
              if ($from eq $to) $fromopt
              else $SomeModule($to)
            case $NoneModule =>
              $NoneModule
          }
        """
    }
    def listTransformer(input: Tree, tpe: Type, nested: (Tree, Type) => Tree): Tree = {
      val fromlist = c.freshName(TermName("fromlist"))
      q"""
          val $fromlist = $input
          var samelist = true
          val tolist = $ListModule.newBuilder[$tpe]
          $fromlist.foreach { src =>
            val dst = ${nested(q"src", tpe)}
            if (src ne dst) samelist = false
            tolist += dst
          }
          if (samelist) $fromlist
          else tolist.result()
        """
    }
    val fname = q"$treeName.${f.name}"
    val rhs = f.tpe match {
      case tpe @ TreeTpe() => treeTransformer(fname, tpe)
      case OptionTreeTpe(tpe) => optionTransformer(fname, tpe, treeTransformer)
      case ListTreeTpe(tpe) => listTransformer(fname, tpe, treeTransformer)
      case _ => fname
    }
    q"val ${TermName(f.name.toString + "1")} = $rhs"
  }

  def leafHandler(l: Leaf, treeName: TermName): Tree = {
    val constructor = hygienicRef(l.sym.companion)
    val hasOnlyPrimitiveFields = l.fields
      .forall(f => f.tpe =:= typeOf[Any] || PrimitiveTpe.unapply(f.tpe))
    if (hasOnlyPrimitiveFields) return q"$treeName"
    val transformedFields: List[ValDef] = l.fields.map(transformField(treeName))

    q"""
      var same = true
      ..$transformedFields
      if (same) $treeName
      else {
        $constructor(..${transformedFields.map(_.name)})
          .withOrigin($OriginModule.PartialProxy($treeName.origin))
      }
    """
  }

  def leafHandlerType(): Tree = TreeClass

  def generatedMethods(): Tree =
    q"""
      private def fail(field: String, from: $TreeClass, to: $TreeClass): $NothingClass = {
        import scala.meta.prettyprinters._
        val errorPrefix = "Invalid transformation of " + field + ": "
        val errorHeader = errorPrefix + from.productPrefix + " -> " + to.productPrefix + ". "
        val errorDetails = "From: " + from.structure + ", to: " + to.structure
        throw new UnsupportedOperationException(errorHeader + errorDetails)
      }
    """
}
