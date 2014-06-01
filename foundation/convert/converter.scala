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
  import internal._

  def converter(annottees: Tree*): Tree = {
    def transform(ddef: DefDef): ModuleDef = {
      val q"$mods def $name[..$tparams](...$paramss): $tpt = $body" = ddef
      val (rawprelude, rawclauses) = body match {
        case q"{ ..$rawprelude; in match { case ..$rawclauses } }" => (rawprelude, rawclauses)
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
      object connector extends Transformer {
        override def transform(tree: Tree): Tree = tree match {
          case DefDef(mods, name, tparams, vparamss, tpt, body) if name != termNames.CONSTRUCTOR =>
            val body1 = atPos(body.pos)(q"_root_.org.scalareflect.convert.internal.connect($wrapper)($body)")
            DefDef(mods, name, tparams, vparamss, tpt, body1)
          case _ =>
            super.transform(tree)
        }
      }
      val prelude = rawprelude.map(connector.transform)
      def normalize(clause: CaseDef): List[CaseDef] = clause match {
        case CaseDef(Alternative(alts), guard, body) => alts.map(alt => atPos(clause.pos)(CaseDef(alt, guard, body)))
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
      case class Instance(in: Tree, out: Tree, clauses: List[Tree], isStub: Boolean) {
        private val prefix = in.toString.replace(".", "") + "2" + out.toString.replace(".", "")
        lazy val decl = c.freshName(TermName(prefix))
        lazy val impl = c.freshName(TermName(prefix))
        def pos = clauses.head.pos
      }
      val instances = mutable.ListBuffer[Instance]()
      clauses.foreach(clause => clause match {
        case cq"$pat if $guard => $body" =>
          val in = intpe(pat)
          val out = outpe(guard)
          val guardlessClause = atPos(clause.pos)(cq"$pat => $body")
          val isStub = body match { case q"unreachable" => true; case q"???" => true; case _ => false }
          val i = instances.indexWhere{ case Instance(iin, iout, _, _) => in.toString == iin.toString && out.toString == iout.toString }
          if (i == -1) instances += Instance(in, out, List(guardlessClause), isStub)
          else instances(i) = Instance(in, out, instances(i).clauses :+ guardlessClause, isStub)
      })
      val precomputeParts = instances.map({
        case Instance(_, _, Nil, _) => unreachable
        case Instance(_, _, clause :: Nil, _) => clause
        case Instance(in, _, clauses, _) => cq"in: $in => in match { case ..$clauses }"
      })
      val precompute = atPos(ddef.pos)(q"""
        _root_.org.scalareflect.convert.internal.precompute($wrapper){
          @_root_.org.scalareflect.convert.internal.names(..${instances.filter(!_.isStub).map(_.impl.toString)})
          def dummy(in: Any): Any = {
            ..$rawprelude
            in match { case ..$precomputeParts }
          }
          ()
        }
      """)
      val instanceDecls = instances.filter(!_.isStub).map(instance => atPos(instance.pos)(
        q"""
          implicit val ${instance.decl} = ${companion.toTypeName}.this.apply((in: ${instance.in}) => {
            ${wrapper.toTypeName}.this.${instance.impl}(in)
          })
        """
      ))
      val instanceImpls = instances.filter(!_.isStub).map(instance => atPos(instance.pos)(
        q"""
          private def ${instance.impl}(in: ${instance.in}) = _root_.org.scalareflect.convert.internal.connect($wrapper) {
            val $helperInstance = new $helperClass(in)
            import $helperInstance._
            ..${prelude.collect { case imp: Import => imp }}
            in match { case ..${instance.clauses} }
          }
        """
      ))

      q"""
        $mods object $wrapper {
          $precompute
          private class $helperClass(in: Any) { ..$prelude }
          trait $typeclass[In, Out] extends _root_.org.scalareflect.convert.Cvt[In, Out]
          object $companion {
            def apply[In, Out](f: In => Out): $typeclass[In, Out] = new $typeclass[In, Out] { def apply(in: In): Out = f(in) }
            import _root_.scala.language.experimental.macros
            implicit def materialize[In, Out]: $typeclass[In, Out] = macro _root_.org.scalareflect.convert.internal.Macros.materialize[In, Out]
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

package object internal {
  case class Converter(in: Any, out: Any, method: Any)
  case class PrecomputeAttachment(converters: List[Converter])

  class names(xs: String*) extends scala.annotation.StaticAnnotation
  def precompute[T](wrapper: Any)(x: T): Unit = macro Macros.precompute
  def connect[T](wrapper: Any)(x: T): Any = macro Macros.connect

  class Macros(val c: Context) {
    import c.universe._
    import definitions._
    import c.internal._
    import decorators._
    def materialize[In: WeakTypeTag, Out: WeakTypeTag]: Tree = {
      ???
    }
    def precompute(wrapper: Tree)(x: Tree): Tree = {
      import c.internal._, decorators._
      val q"{ ${dummy @ q"def $_(in: $_): $_ = { ..$prelude; in match { case ..$clauses } }"}; () }" = x
      def toPalladiumConverters: List[Converter] = {
        def precisetpe(tree: Tree): Type = {
          // NOTE: postprocessing is not mandatory, but it's sort of necessary to simplify types
          // due to the fact that we have `type ThisType` in every root, branch and leaf node
          // lubs of node types can have really baroque ThisType refinements :)
          def postprocess(lub: Type): Type = removeThisTypeFromRefinement(lub)
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
        def validateExhaustiveInputs(): Unit = {
          val root = typeOf[scala.tools.nsc.Global]
          def ref(sym: Symbol): Type = sym match {
            case csym: ClassSymbol => csym.toType
            case msym: ModuleSymbol => msym.info
            case _ => NoType
          }
          def sortOfAllSubclassesOf(tpe: Type): List[Symbol] = root.members.toList.flatMap(sym => if ((ref(sym) <:< tpe) && !sym.isAbstract) Some(sym) else None)
          val expected = sortOfAllSubclassesOf(typeOf[scala.reflect.internal.Trees#Tree]) ++ sortOfAllSubclassesOf(typeOf[scala.reflect.internal.Types#Type])
          val inputs = clauses.map(_.pat.tpe).map(tpe => tpe.termSymbol.orElse(tpe.typeSymbol)).map(sym => root.member(sym.name))
          val unmatched = expected.filter(exp => !inputs.exists(pat => ref(exp) <:< ref(pat)))
          if (unmatched.nonEmpty) c.abort(c.enclosingPosition, "@converter is not exhaustive in its inputs; missing: " + unmatched)
        }
        def validateExhaustiveOutputs(): Unit = {
          val root = typeOf[scala.reflect.core.Tree].typeSymbol.asClass
          def allLeafCompanions(csym: ClassSymbol): List[Symbol] = {
            val _ = csym.info // workaround for a knownDirectSubclasses bug
            if (csym.isSealed) csym.knownDirectSubclasses.toList.map(_.asClass).flatMap(allLeafCompanions)
            else if (csym.isFinal) {
              // somehow calling companionSymbol on results of knownDirectSubclasses is flaky
              val companionSymbol = csym.owner.info.member(csym.name.toTermName)
              if (companionSymbol == NoSymbol) c.abort(c.enclosingPosition, "companionless leaf in @root hierarchy")
              List(csym.companionSymbol)
            } else if (csym.isModuleClass) {
              // haven't encountered bugs here, but just in case
              if (csym.module == NoSymbol) c.abort(c.enclosingPosition, "moduleless leaf in @root hierarchy")
              List(csym.module)
            } else c.abort(c.enclosingPosition, "open class in a @root hierarchy: " + csym)
          }
          val expected = mutable.Set(allLeafCompanions(root).distinct: _*)
          (prelude ++ clauses).foreach(_.foreach(sub => if (sub.symbol != null) expected -= sub.symbol))
          val unmatched = expected.filter(sym => {
            sym.fullName != "scala.reflect.core.Arg.Named" &&
            sym.fullName != "scala.reflect.core.Arg.Repeated" &&
            sym.fullName != "scala.reflect.core.Aux.CompUnit" &&
            sym.fullName != "scala.reflect.core.Decl.Procedure" &&
            sym.fullName != "scala.reflect.core.Defn.Procedure" &&
            sym.fullName != "scala.reflect.core.Enum.Generator" &&
            sym.fullName != "scala.reflect.core.Enum.Guard" &&
            sym.fullName != "scala.reflect.core.Enum.Val" &&
            sym.fullName != "scala.reflect.core.Lit.Symbol" &&
            sym.fullName != "scala.reflect.core.Mod.Doc" &&
            sym.fullName != "scala.reflect.core.Mod.ValParam" &&
            sym.fullName != "scala.reflect.core.Mod.VarParam" &&
            sym.fullName != "scala.reflect.core.Pat.ExtractInfix" &&
            sym.fullName != "scala.reflect.core.Pat.Interpolate" &&
            sym.fullName != "scala.reflect.core.Pat.Tuple" &&
            sym.fullName != "scala.reflect.core.Term.Annotate" &&
            sym.fullName != "scala.reflect.core.Term.ApplyInfix" &&
            sym.fullName != "scala.reflect.core.Term.ApplyUnary" &&
            sym.fullName != "scala.reflect.core.Term.Do" &&
            sym.fullName != "scala.reflect.core.Term.Eta" &&
            sym.fullName != "scala.reflect.core.Term.For" &&
            sym.fullName != "scala.reflect.core.Term.ForYield" &&
            sym.fullName != "scala.reflect.core.Term.If.Then" &&
            sym.fullName != "scala.reflect.core.Term.Interpolate" &&
            sym.fullName != "scala.reflect.core.Term.Placeholder" &&
            sym.fullName != "scala.reflect.core.Term.Tuple" &&
            sym.fullName != "scala.reflect.core.Term.Update" &&
            sym.fullName != "scala.reflect.core.Term.While" &&
            sym.fullName != "scala.reflect.core.Type.ApplyInfix" &&
            sym.fullName != "scala.reflect.core.Type.Function" &&
            sym.fullName != "scala.reflect.core.Type.Placeholder" &&
            sym.fullName != "scala.reflect.core.Type.Tuple"
          })
          if (unmatched.nonEmpty) c.abort(c.enclosingPosition, "@converter is not exhaustive in its outputs; missing: " + unmatched)
        }
        // val tups = clauses.map{ case CaseDef(pat, _, body) => (pat.tpe.toString.replace("HostContext.this.", ""), precisetpe(body).toString.replace("scala.reflect.core.", "p.")) }
        // val max = tups.map(_._1.length).max
        // tups.foreach{ case (f1, f2) => println(f1 + (" " * (max - f1.length + 5)) + f2) }
        clauses.foreach(validateAllowedPattern)
        validateExhaustiveInputs()
        validateExhaustiveOutputs()
        val unreachableMeth = typeOf[org.scalareflect.`package`.type].member(TermName("unreachable"))
        val qqqMeth = typeOf[Predef.type].member(TermName("$qmark$qmark$qmark"))
        val relevantClauses = clauses.filter(clause => clause.body.symbol != unreachableMeth && clause.body.symbol != qqqMeth)
        val ins = relevantClauses.map(_.pat.tpe)
        val outs = relevantClauses.map(_.body).map(precisetpe)
        val methods = dummy.symbol.annotations.head.scalaArgs.map{
          case Literal(Constant(s: String)) =>
            val methodSym = wrapper.symbol.info.member(TermName(s)).orElse(c.abort(c.enclosingPosition, s"something went wrong: can't resolve $s in $wrapper"))
            q"$wrapper.$methodSym".duplicate
        }
        if (ins.length != outs.length || outs.length != methods.length) c.abort(c.enclosingPosition, s"something went wrong: can't create converters from ${ins.length}, ${outs.length} and ${methods.length}")
        ins.zip(outs).zip(methods).map{ case ((in, out), method) => Converter(in, out, method) }
      }
      val target = wrapper.symbol.asModule.moduleClass.orElse(c.abort(c.enclosingPosition, s"something went wrong: unexpected wrapper $wrapper"))
      val converters = target.name.toString match {
        case "toPalladium" => toPalladiumConverters
        case _ => c.abort(c.enclosingPosition, "unknown target: " + target.name)
      }
      target.updateAttachment(PrecomputeAttachment(converters))
      q""
    }
    def connect(wrapper: Tree)(x: Tree): Tree = {
      case class Converter(in: Type, out: Type, method: Tree)
      def obtainConverters(sym: Symbol): List[Converter] = {
        if (sym == NoSymbol) c.abort(c.enclosingPosition, "something went wrong: can't obtain PrecomputeAttachment")
        val att = sym.attachments.get[PrecomputeAttachment]
        val result = att.map(_.converters.map(cvtr => Converter(cvtr.in.asInstanceOf[Type], cvtr.out.asInstanceOf[Type], cvtr.method.asInstanceOf[Tree])))
        result.getOrElse(obtainConverters(sym.owner))
      }
      val converters = obtainConverters(wrapper.symbol.asModule.moduleClass)
      object transformer {
        val Predef_??? = typeOf[Predef.type].member(TermName("$qmark$qmark$qmark")).asMethod
        val List_apply = typeOf[List.type].member(TermName("apply")).asMethod
        val Some_apply = typeOf[Some.type].member(TermName("apply")).asMethod
        def mkAttributedApply(m: MethodSymbol, arg: Tree): Apply = {
          val owner = if (m.owner.isModuleClass) m.owner.asClass.module else m.owner
          val pre = gen.mkAttributedRef(owner)
          val polysel = gen.mkAttributedSelect(pre, m)
          val sel = TypeApply(polysel, List(TypeTree(arg.tpe))).setType(appliedType(polysel.tpe, arg.tpe))
          Apply(sel, List(arg)).setType(sel.tpe.finalResultType)
        }
        var pt: Type = NoType
        def refinePt(upperbound: Type): Unit = {
          if (pt != NoType) pt = removeThisTypeFromRefinement(lub(List(pt, upperbound)))
          else if (upperbound != NothingTpe) pt = upperbound
          else pt = NoType
        }
        def transform(tree: Tree): Tree = typingTransform(tree)((tree, api) => {
          def convert(convertee: Tree): Tree = {
            println(convertee.tpe.widen + " -> " + pt)
            gen.mkAttributedRef(Predef_???).setType(NothingTpe)
          }
          def transformApplyCalculatingPts(app: Apply): Apply = {
            treeCopy.Apply(app, api.recur(app.fun), app.args.zipWithIndex.map({ case (arg, i) =>
              val params = app.fun.tpe.paramss.head
              def isVararg = i >= params.length - 1 && params.last.info.typeSymbol == RepeatedParamClass
              def unVararg(tpe: Type) = tpe.typeArgs.head
              pt = if (isVararg) unVararg(params.last.info) else params(i).info
              val result = api.recur(arg)
              pt = NoType
              result
            }))
          }
          // TODO: think whether we can rebind polymorphic usages of cvt in a generic fashion
          // without hardcoding to every type constructor we're going to use
          tree match {
            case q"$_.List.apply[${t: Type}]($_($convertee).cvt)" =>
              pt = if (pt != NoType) pt.typeArgs.head else pt
              refinePt(t)
              mkAttributedApply(List_apply, convert(convertee))
            case q"$_.Some.apply[${t: Type}]($_($convertee).cvt)" =>
              pt = if (pt != NoType) pt.typeArgs.head else pt
              refinePt(t)
              mkAttributedApply(Some_apply, convert(convertee))
            case q"$_($convertee).cvt.asInstanceOf[${t: Type}]" =>
              refinePt(t)
              convert(convertee)
            case q"$_($convertee).cvt" =>
              convert(convertee)
            case app: Apply =>
              transformApplyCalculatingPts(app)
            case _ =>
              pt = NoType
              api.default(tree)
          }
        })
      }
      transformer.transform(x).setType(removeThisTypeFromRefinement(x.tpe))
    }
    def removeThisTypeFromRefinement(tpe: Type): Type = {
      object Scope { def unapplySeq(scope: Scope): Some[Seq[Symbol]] = Some(scope.toList) }
      object ThisType { def unapply(sym: Symbol): Boolean = sym.name == TypeName("ThisType") }
      tpe match {
        case RefinedType(underlying :: Nil, Scope(ThisType())) => underlying
        case RefinedType(parents, Scope(ThisType())) => refinedType(parents, newScopeWith())
        case lub => lub
      }
    }
  }
}
