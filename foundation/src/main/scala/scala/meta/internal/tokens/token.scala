package scala.meta
package internal
package tokens

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import org.scalameta.internal.MacroHelpers

// @token is a specialized version of @org.scalameta.adt.leaf for scala.meta tokens.
class token(name: String) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro TokenNamerMacros.impl
}

class TokenNamerMacros(val c: Context) extends MacroHelpers {
  import c.universe._

  val Unsupported = tq"_root_.scala.`package`.UnsupportedOperationException"
  val Content = tq"_root_.scala.meta.inputs.Content"
  val Dialect = tq"_root_.scala.meta.Dialect"
  val Token = tq"_root_.scala.meta.tokens.Token"
  val Classifier = tq"_root_.scala.meta.classifiers.Classifier"

  def impl(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
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

      // step 1: generate boilerplate required by the @adt infrastructure
      anns1 += q"new $AdtPackage.leaf(toString = false)"
      anns1 += q"new $TokenMetadataModule.tokenClass"
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

      // step 3: generate implementation of `def name: String`
      // TODO: deduplicate with scala.meta.internal.prettyprinters.escape
      // NOTE: can be removed altogether once tokens are renamed
      val codepage = Map("\t" -> "\\t", "\b" -> "\\b", "\n" -> "\\n", "\r" -> "\\r", "\f" -> "\\f", "\\" -> "\\\\")
      val tokenName = providedTokenName.flatMap(c => codepage.getOrElse(c.toString, c.toString))
      stats1 += q"def name: _root_.scala.Predef.String = $tokenName"

      // step 4: generate implementation of  `def end: String` for static tokens
      val isStaticToken = !paramss.flatten.exists(_.name.toString == "end")
      val needsEnd = isStaticToken && !stats.exists{ case DefDef(_, TermName("end"), _, _, _, _) => true; case _ => false }
      if (needsEnd) {
        var code = name.decodedName.toString
        if (code == "_ ") code = "_" // NOTE: can't call a class `_`, so have to use `_ `
        if (code == "class ") code = "class" // TODO: wat?
        if (code == "package ") code = "package" // TODO: wat?
        val hasCustomCode = isStaticToken && stats.exists{ case DefDef(_, TermName("code"), _, _, _, _) => true; case _ => false }
        val codeRef = if (hasCustomCode) q"this.code.length" else q"${code.length}"
        stats1 += q"def end: _root_.scala.Int = this.start + $codeRef"
      }

      // step 5: generate implementation of `def adjust`
      val needsAdjust = !stats.exists{ case DefDef(_, TermName("adjust"), _, _, _, _) => true; case _ => false }
      if (needsAdjust) {
        val paramContent = q"val content: $Content = this.content"
        val paramDialect = q"val dialect: $Dialect = this.dialect"
        val paramStart = q"val start: _root_.scala.Int = this.start"
        val paramEnd = q"val end: _root_.scala.Int = this.end"
        val paramDelta = q"val delta: _root_.scala.Int = 0"
        val adjustResult = {
          if (name.toString == "BOF" || name.toString == "EOF") q"this.copy(content = content, dialect = dialect)"
          else if (isStaticToken) q"this.copy(content = content, dialect = dialect, start = start)"
          else q"this.copy(content = content, dialect = dialect, start = start, end = end)"
        }
        val adjustError = {
          if (name.toString == "BOF" || name.toString == "EOF") q""" "position-changing adjust on Token." + this.name """
          else if (isStaticToken) q""" "end-changing adjust on Tokens." + this.name """
          else q""" "fatal error in the token infrastructure" """
        }
        val body = q"""
          (start != this.start || end != this.end, delta != 0) match {
            case (false, false) =>
              this.copy(content = content, dialect = dialect)
            case (true, false) =>
              val result = $adjustResult
              if (result.start != start || result.end != end) {
                var message = $adjustError
                message += (": expected " + result.start + ".." + result.end)
                message += (", actual " + start + ".." + end)
                throw new $Unsupported(message)
              }
              result
            case (false, true) =>
              this.adjust(content = content, dialect = dialect, start = this.start + delta, end = this.end + delta)
            case (true, true) =>
              throw new $Unsupported("you can specify either start/end or delta, but not both")
          }
        """
        stats1 += q"def adjust($paramContent, $paramDialect, $paramStart, $paramEnd, $paramDelta): $Token = $body"
      }

      // step 6: generate the boilerplate fields
      var paramss1 = (q"val content: $Content" +: q"val dialect: $Dialect" +: paramss.head) +: paramss.tail
      val cdef1 = q"$mods1 class $name[..$tparams] $ctorMods(...$paramss1) extends { ..$earlydefns } with ..$parents { $self => ..$stats1 }"
      val mdef1 = q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      List(cdef1, mdef1)
    }
  })
}