package scala.meta
package internal
package tokens

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import org.scalameta.internal.MacroHelpers

// @freeform and @fixed are specialized versions of @org.scalameta.adt.leaf for scala.meta tokens.

class freeform(name: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TokenNamerMacros.freeform
}

class fixed(name: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TokenNamerMacros.fixed
}

class TokenNamerMacros(val c: Context) extends MacroHelpers {
  import c.universe._

  val Unsupported = tq"_root_.scala.`package`.UnsupportedOperationException"
  val Dialect = tq"_root_.scala.meta.Dialect"
  val Input = tq"_root_.scala.meta.inputs.Input"
  val Token = tq"_root_.scala.meta.tokens.Token"
  val OptionsClass = tq"_root_.scala.meta.prettyprinters.Syntax.Options"
  val OptionsModule = q"_root_.scala.meta.prettyprinters.Syntax.Options"
  val Classifier = tq"_root_.scala.meta.classifiers.Classifier"
  val Int = tq"_root_.scala.Int"
  val PositionClass = tq"_root_.scala.meta.inputs.Position"
  val PositionModule = q"_root_.scala.meta.inputs.Position"
  val PointClass = tq"_root_.scala.meta.inputs.Point"
  val PointModule = q"_root_.scala.meta.inputs.Point"

  def freeform(annottees: Tree*): Tree = impl(annottees, isFixed = false)

  def fixed(annottees: Tree*): Tree = impl(annottees, isFixed = true)

  private def impl(annottees: Seq[Tree], isFixed: Boolean): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      val q"new $_(...$argss).macroTransform(..$_)" = c.macroApplication
      val providedTokenName = argss match {
        case List(List(Literal(Constant(tokenName: String)))) => tokenName
        case _ => c.abort(c.enclosingPosition, "@token annotation takes a literal string argument")
      }

      val q"$mods class $name[..$tparams] $ctorMods(...$paramss) extends { ..$earlydefns } with ..$parents { $self => ..$stats }" = cdef
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val stats1 = ListBuffer[Tree]() ++ stats
      val anns1 = ListBuffer[Tree]() ++ mods.annotations
      def mods1 = mods.mapAnnotations(_ => anns1.toList)
      val parents1 = ListBuffer[Tree]() ++ parents
      val mstats1 = ListBuffer[Tree]() ++ mstats
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)
      def hasMethod(name: String): Boolean = stats1.exists{ case DefDef(_, TermName(`name`), _, _, _, _) => true; case _ => false }

      // step 1: generate boilerplate required by the @adt infrastructure
      // NOTE: toString is inherited from Token, unapply is customized.
      anns1 += q"new $AdtPackage.leaf(toString = false, apply = false, unapply = false)"
      anns1 += q"new $TokenMetadataModule.tokenClass(name = $providedTokenName, freeform = ${!isFixed})"
      manns1 += q"new $TokenMetadataModule.tokenCompanion"

      // step 2: generate boilerplate required by the classifier infrastructure
      val q"..$classifierBoilerplate" = q"""
        private object sharedClassifier extends $Classifier[$Token, $name] {
          def apply(x: $Token): Boolean = x.isInstanceOf[$name]
        }
        implicit def classifier[T <: $Token]: $Classifier[T, $name] = {
          sharedClassifier.asInstanceOf[$Classifier[T, $name]]
        }
      """
      mstats1 ++= classifierBoilerplate

      // step 3: perform manual mixin composition in order to avoid the creation of Token$class.class.
      // We kinda have to do that, because we want to have a `Token.Class` class.
      stats1 += q"""
        def pos: $PositionClass = $PositionModule.Range(this.input, this.start, this.end)
      """
      stats1 += q"""
        final override def toString: $StringClass = _root_.scala.meta.internal.prettyprinters.TokenToString(this)
      """

      // step 4: generate implementation of `def name: String`
      // TODO: deduplicate with scala.meta.internal.prettyprinters.escape
      val codepage = Map("\t" -> "\\t", "\b" -> "\\b", "\n" -> "\\n", "\r" -> "\\r", "\f" -> "\\f", "\\" -> "\\\\")
      val tokenName = providedTokenName.flatMap(c => codepage.getOrElse(c.toString, c.toString))
      stats1 += q"def name: _root_.scala.Predef.String = $tokenName"

      // step 5: generate implementation of `def end: String` for fixed tokens
      if (isFixed && !hasMethod("end")) {
        val code = providedTokenName // as simple as that
        stats1 += q"def end: _root_.scala.Int = this.start + ${code.length}"
      }

      // step 6: generate implementation of `Companion.unapply`
      // TODO: deduplicate wrt @data
      val unapplyParams = paramss.head
      if (unapplyParams.length != 0) {
        val successTargs = unapplyParams.map(_.tpt)
        val successTpe = tq"(..$successTargs)"
        val successArgs = q"(..${unapplyParams.map(p => q"x.${p.name}")})"
        mstats1 += q"""
          def unapply(x: $name): Option[$successTpe] = {
            if (x == null) _root_.scala.None
            else _root_.scala.Some($successArgs)
          }
        """
      } else {
        mstats1 += q"def unapply(x: $name): Boolean = true"
      }

      // step 7: generate boilerplate parameters
      var boilerplateParams = List(q"val input: $Input", q"val dialect: $Dialect")
      if (!hasMethod("start")) boilerplateParams :+= q"val start: $Int"
      if (!hasMethod("end")) boilerplateParams :+= q"val end: $Int"
      var paramss1 = (boilerplateParams ++ paramss.head) +: paramss.tail

      // step 8: generate implementation of `Companion.apply`
      // TODO: deduplicate wrt @data
      val applyParamss = paramss1.map(_.map(_.duplicate))
      val applyArgss = paramss1.map(_.map(p => q"${p.name}"))
      mstats1 += q"private[meta] def apply(...$applyParamss): $name = new $name(...$applyArgss)"

      val cdef1 = q"$mods1 class $name[..$tparams] $ctorMods(...$paramss1) extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
      val mdef1 = q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }
  })
}