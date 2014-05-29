package org.scalareflect
package convert

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable
import org.scalareflect.unreachable

class converter extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro ConverterMacros.converter
}

class ConverterMacros(val c: Context) {
  import c.universe._

  def converter(annottees: Tree*): Tree = {
    def transform(ddef: DefDef): ModuleDef = {
      val q"$mods def $name[..$tparams](...$paramss): $tpt = $body" = ddef
      val (prelude, rawclauses) = body match {
        case q"{ ..$prelude; in match { case ..$rawclauses } }" => (prelude, rawclauses)
        case _ => c.abort(c.enclosingPosition, "@converter methods must end in a pattern match")
      }
      if (tparams.nonEmpty) c.abort(c.enclosingPosition, "@converter methods must not define type parameters")
      if (paramss.length != 1 || paramss(0).length != 2) c.abort(c.enclosingPosition, "@converter methods must define just two parameters: (in: Any, pt: ru.Type)")
      val returnTypeIsAny = tpt match { case tq"Any" => true; case _ => false }
      if (!returnTypeIsAny) c.abort(c.enclosingPosition, "@converter methods must define their return type as Any")
      rawclauses foreach {
        case cq"$_ => $_" => // ok
        case cq"$_ if pt <:< typeOf[$_] => $_" => // ok
        case cq"$_ if $guard => $_" => c.abort(guard.pos, "@converter matches must not use guards except for `pt <:< typeOf[...]`")
      }

      val wrapper = name
      val typeclass = TypeName(name.toString.capitalize + "Cvt")
      val companion = typeclass.toTermName
      val helperClass = c.freshName(TypeName(name.toString.capitalize + "Helper"))
      val helperInstance = c.freshName(TermName(name.toString.capitalize + "Helper"))
      def normalize(clause: CaseDef): List[CaseDef] = clause match {
        case CaseDef(Alternative(alts), guard, body) => alts.map(CaseDef(_, guard, body))
        case _ => List(clause)
      }
      val clauses = rawclauses.flatMap(normalize)
      def typeref(termref: Tree): Tree = termref match {
        case Ident(name @ TermName(_)) => Ident(name.toTypeName)
        case Select(qual, name @ TermName(_)) => Select(qual, name.toTypeName)
      }
      def intpe(pat: Tree): Tree = pat match {
        case Ident(_) | Select(_, _) => pat
        case Bind(_, body) => intpe(body)
        case Apply(Select(Apply(Ident(TermName("StringContext")), _), _), _) => c.abort(pat.pos, "@converter matches must use manual tree deconstruction")
        case Apply(fn, _) => typeref(fn)
        case Typed(_, tpe) => tpe
      }
      def outpe(guard: Tree): Tree = guard match {
        case q"pt <:< typeOf[$tpe]" => tpe
        case _ => tq"p.Tree"
      }
      case class Instance(in: Tree, out: Tree, clauses: List[Tree]) {
        private val prefix = in.toString.replace(".", "") + "2" + out.toString.replace(".", "")
        lazy val decl = c.freshName(TermName(prefix))
        lazy val impl = c.freshName(TermName(prefix))
      }
      val instances = mutable.ListBuffer[Instance]()
      clauses.foreach(clause => clause match {
        case cq"$pat if $guard => $body" =>
          val in = intpe(pat)
          val out = outpe(guard)
          val clause = cq"$pat => $body"
          val i = instances.indexWhere{ case Instance(iin, iout, _) => in.toString == iin.toString && out.toString == iout.toString }
          if (i == -1) instances += Instance(in, out, List(clause))
          else instances(i) = Instance(in, out, instances(i).clauses :+ clause)
      })
      val precomputeParts = instances.map({
        case Instance(_, _, Nil) => unreachable
        case Instance(_, _, clause :: Nil) => clause
        case Instance(in, _, clauses) => cq"in: $in => in match { case ..$clauses }"
      })
      val precompute = q"""
        _root_.org.scalareflect.convert.ConvertInternal.precompute[$wrapper.type]({
          def $name(in: Any): Any = {
            ..$prelude
            in match { case ..$precomputeParts }
          }
          ()
        })
      """
      val instanceDecls = instances.map({ instance =>
        q"""
          implicit val ${instance.decl} = ${companion.toTypeName}.this.apply((in: ${instance.in}) => {
            ${wrapper.toTypeName}.this.${instance.impl}(in)
          })
        """
      })
      val instanceImpls = instances.map({ case instance =>
        q"""
          private def ${instance.impl}(in: ${instance.in}) = {
            val $helperInstance = new $helperClass(in)
            import $helperInstance._
            ..${prelude.collect { case imp: Import => imp }}
            in match { case ..${instance.clauses} }
          }
        """
      })

      q"""
        $mods object $wrapper {
          $precompute
          private class $helperClass(in: Any) { ..$prelude }
          trait $typeclass[In, Out] extends _root_.org.scalareflect.convert.Cvt[In, Out]
          object $companion {
            def apply[In, Out](f: In => Out): $typeclass[In, Out] = new $typeclass[In, Out] { def apply(in: In): Out = f(in) }
            import _root_.scala.language.experimental.macros
            implicit def materialize[In, Out]: $typeclass[In, Out] = macro _root_.org.scalareflect.convert.ConvertInternalMacro.materialize[In, Out]
            ..$instanceDecls
          }
          ..$instanceImpls
          def apply[In, Out](x: In)(implicit ev: $typeclass[In, Out]): Out = ev(x)
          def apply[In, Pt >: Out, Out](x: In, pt: _root_.java.lang.Class[Pt])(implicit ev: $typeclass[In, Out]): Out = ev(x)
        }
      """
    }
    val expanded = annottees match {
      case (ddef: DefDef) :: rest => transform(ddef) :: rest
      case annottee :: rest => c.abort(annottee.pos, "only methods can be @converter")
    }
    q"{ ..$expanded; () }"
  }
}

case class PrecomputeAttachment(outs: List[Any])
object ConvertInternal {
  def precompute[T](x: Any): Unit = macro ConvertInternalMacro.precompute[T]
}
class ConvertInternalMacro(val c: Context) {
  import c.universe._
  def materialize[In: WeakTypeTag, Out: WeakTypeTag]: Tree = {
    ???
  }
  def precompute[T: WeakTypeTag](x: Tree): Tree = {
    import c.internal._, decorators._
    val q"{ def $_(in: $_): $_ = { ..$_; in match { case ..$clauses } }; () }" = x
    def toPalladiumTpes: List[Type] = {
      def precisetpe(tree: Tree): Type = {
        // NOTE: postprocessing is not mandatory, but it's sort of necessary to simplify types
        // due to the fact that we have `type ThisType` in every root, branch and leaf node
        // lubs of node types can have really baroque ThisType refinements :)
        object Scope { def unapplySeq(scope: Scope): Some[Seq[Symbol]] = Some(scope.toList) }
        object ThisType { def unapply(sym: Symbol): Boolean = sym.name == TypeName("ThisType") }
        def postprocess(lub: Type): Type = lub match {
          case RefinedType(underlying :: Nil, Scope(ThisType())) => underlying
          case RefinedType(parents, Scope(ThisType())) => refinedType(parents, newScopeWith())
          case lub => lub
        }
        tree match {
          case If(_, thenp, elsep) => postprocess(lub(List(precisetpe(thenp), precisetpe(elsep))))
          case Match(_, cases) => postprocess(lub(cases.map(tree => precisetpe(tree.body))))
          case Block(_, expr) => precisetpe(expr)
          case tree => tree.tpe
        }
      }
      def validateAllowedPattern(clause: CaseDef): CaseDef = {
        val in = clause.pat.tpe.typeSymbol.asClass
        if (in.baseClasses.exists(sym => sym.fullName == "scala.reflect.internal.Trees.Tree" ||
                                         sym.fullName == "scala.reflect.internal.Types.Type")) clause
        else c.abort(clause.pos, "must only convert from Scala trees and types")
      }
      def validateExhaustivePatterns(): Unit = {
        val root = typeOf[scala.tools.nsc.Global]
        def ref(sym: GSymbol): Type = sym match {
          case csym: ClassSymbol => csym.toType
          case msym: ModuleSymbol => msym.info
          case _ => NoType
        }
        def sortOfAllSubclassesOf(tpe: Type): List[Symbol] = root.members.toList.flatMap(sym => if (ref(sym) <:< tpe) Some(sym) else None)
        val expected = sortOfAllSubclassesOf(typeOf[scala.reflect.internal.Trees#Tree]) ++ sortOfAllSubclassesOf(typeOf[scala.reflect.internal.Types#Type])
        val patterns = clauses.map(_.pat.tpe).map(tpe => tpe.termSymbol.orElse(tpe.typeSymbol)).map(sym => root.member(sym.name))
        val unmatched = expected.filter(exp => !patterns.exists(pat => ref(exp) <:< ref(pat)))
        unmatched foreach println
      }
      // val tups = clauses.map{ case CaseDef(pat, _, body) => (pat.tpe.toString.replace("HostContext.this.", ""), precisetpe(body).toString.replace("scala.reflect.core.", "p.")) }
      // val max = tups.map(_._1.length).max
      // tups.foreach{ case (f1, f2) => println(f1 + (" " * (max - f1.length + 5)) + f2) }
      validateExhaustivePatterns()
      clauses.map(validateAllowedPattern).map(_.body).map(precisetpe)
    }
    val target = weakTypeOf[T].typeSymbol
    val tpes = target.name.toString match {
      case "toPalladium" => toPalladiumTpes
      case _ => c.abort(c.enclosingPosition, "unknown target: " + target.name)
    }
    target.updateAttachment(PrecomputeAttachment(tpes))
    q""
  }
}
