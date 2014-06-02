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
            val body1 = atPos(body.pos)(q"_root_.org.scalareflect.convert.internal.connectConverters($body)")
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
        case _ => EmptyTree
      }
      case class Instance(in: Tree, out: Tree, clauses: List[Tree], notImplemented: Boolean, notBounded: Boolean) {
        private val prefix = in.toString.replace(".", "") + "2" + (if (notBounded) "Wildcard" else out.toString.replace(".", ""))
        lazy val decl = c.freshName(TermName(prefix))
        lazy val impl = c.freshName(TermName(prefix))
        def pos = clauses.head.pos
      }
      val instances = mutable.ListBuffer[Instance]()
      clauses.foreach(clause => clause match {
        case cq"$pat if $guard => $body" =>
          val in = intpe(pat)
          val out = outpe(guard).orElse(tq"p.Tree")
          val guardlessClause = atPos(clause.pos)(cq"$pat => $body")
          val notImplemented = body match { case q"unreachable" => true; case q"???" => true; case _ => false }
          val notBounded = outpe(guard).isEmpty
          val i = instances.indexWhere{ case Instance(iin, iout, _, _, _) => in.toString == iin.toString && out.toString == iout.toString }
          if (i == -1) instances += Instance(in, out, List(guardlessClause), notImplemented, notBounded)
          else instances(i) = Instance(in, out, instances(i).clauses :+ guardlessClause, notImplemented, notBounded)
      })
      val computeParts = instances.map({
        case Instance(_, _, Nil, _, _) => unreachable
        case Instance(_, _, clause :: Nil, _, _) => clause
        case Instance(in, _, clauses, _, _) => atPos(clauses.head.pos)(cq"in: $in => in match { case ..$clauses }")
      })
      val computeConverters = atPos(ddef.pos)(q"""
        _root_.org.scalareflect.convert.internal.computeConverters($wrapper){
          @_root_.org.scalareflect.convert.internal.names(..${instances.filter(!_.notImplemented).map(_.impl.toString)})
          def dummy(in: Any): Any = {
            ..$rawprelude
            in match { case ..$computeParts }
          }
          ()
        }
      """)
      val instanceDecls = instances.filter(!_.notImplemented).map(instance => atPos(instance.pos)(
        q"""
          implicit val ${instance.decl} = ${companion.toTypeName}.this.apply((in: ${instance.in}) => {
            ${wrapper.toTypeName}.this.${instance.impl}(in)
          })
        """
      ))
      val instanceImpls = instances.filter(!_.notImplemented).map(instance => atPos(instance.pos)(
        q"""
          private def ${instance.impl}(in: ${instance.in}) = _root_.org.scalareflect.convert.internal.connectConverters {
            val $helperInstance = new $helperClass(in)
            import $helperInstance._
            ..${prelude.collect { case imp: Import => imp }}
            in match { case ..${instance.clauses} }
          }
        """
      ))

      q"""
        $mods object $wrapper {
          $computeConverters
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
  case class Converter(in: Any, out: Any, method: Any, derived: Boolean)
  case class ComputedConvertersAttachment(converters: List[Converter])

  class names(xs: String*) extends scala.annotation.StaticAnnotation
  def computeConverters[T](wrapper: Any)(x: T): Unit = macro Macros.computeConverters
  def connectConverters[T](x: T): Any = macro Macros.connectConverters

  class Macros(val c: Context) {
    import c.universe._
    import definitions._
    import c.internal._
    import decorators._
    val Predef_??? = typeOf[Predef.type].member(TermName("$qmark$qmark$qmark")).asMethod
    val List_apply = typeOf[List.type].member(TermName("apply")).asMethod
    val Some_apply = typeOf[Some.type].member(TermName("apply")).asMethod
    val Scalareflect_unreachable = typeOf[org.scalareflect.`package`.type].member(TermName("unreachable")).asMethod
    val Convert_derive = typeOf[org.scalareflect.convert.`package`.type].member(TermName("derive")).asMethod
    val SeqClass = symbolOf[scala.collection.immutable.Seq[_]]
    val RichCvt_cvt = typeOf[org.scalareflect.convert.`package`.RichCvt].member(TermName("cvt")).asMethod
    val Any_asInstanceOf = AnyTpe.member(TermName("asInstanceOf")).asMethod
    val LeafAnnotation = symbolOf[org.scalareflect.adt.Internal.leaf]
    object Cvt {
      def unapply(x: Tree): Option[(Tree, Type, Boolean)] = {
        object RawCvt {
          def unapply(x: Tree): Option[Tree] = x match {
            case q"$_($convertee).$_" if x.symbol == RichCvt_cvt => Some(convertee)
            case _ => None
          }
        }
        object Ascribe {
          def unapply(x: Tree): Option[(Tree, Type)] = x match {
            case Typed(x, pt) => Some((x, pt.tpe))
            case _ => None
          }
        }
        object Cast {
          def unapply(x: Tree): Option[(Tree, Type)] = x match {
            case TypeApply(sel @ Select(x, _), List(pt)) if sel.symbol == Any_asInstanceOf => Some((x, pt.tpe))
            case _ => None
          }
        }
        x match {
          case RawCvt(convertee) => Some((convertee, WildcardType, false))
          case Ascribe(RawCvt(convertee), pt) => Some((convertee, pt, false))
          case Cast(RawCvt(convertee), pt) => Some((convertee, pt, false))
          case q"$_($convertee).$_: ${t: Type}" if x.symbol == RichCvt_cvt => Some((convertee, WildcardType, false))
          case _ => None
        }
      }
    }
    def materialize[In: WeakTypeTag, Out: WeakTypeTag]: Tree = {
      ???
    }
    case class Converter(in: Type, out: Type, method: Tree, derived: Boolean)
    type SharedConverter = org.scalareflect.convert.internal.Converter
    val SharedConverter = org.scalareflect.convert.internal.Converter
    def computeConverters(wrapper: Tree)(x: Tree): Tree = {
      import c.internal._, decorators._
      val q"{ ${dummy @ q"def $_(in: $_): $_ = { ..$prelude; in match { case ..$clauses } }"}; () }" = x
      def toPalladiumConverters: List[SharedConverter] = {
        def precisetpe(tree: Tree): Type = tree match {
          case If(_, thenp, elsep) => lub(List(precisetpe(thenp), precisetpe(elsep)))
          case Match(_, cases) => lub(cases.map(tree => precisetpe(tree.body)))
          case Block(_, expr) => precisetpe(expr)
          case tree => tree.tpe
        }
        def validateAllowedInputs(): Boolean = {
          var isValid = true
          clauses.foreach(clause => {
            val in = clause.pat.tpe.typeSymbol.asClass
            val inIsTreeOrType = in.baseClasses.exists(sym => sym.fullName == "scala.reflect.internal.Trees.Tree" ||
                                                              sym.fullName == "scala.reflect.internal.Types.Type")
            if (!inIsTreeOrType) {
              c.error(clause.pos, s"must only convert from Scala trees and types, found $in")
              isValid = false
            }
          })
          isValid
        }
        def validateAllowedOutputs(): Boolean = {
          var isValid = true
          clauses.foreach(clause => {
            val body = clause.body
            if (body.symbol != Scalareflect_unreachable && body.symbol != Predef_??? && body.symbol != Convert_derive) {
              val tpe = precisetpe(body)
              if (tpe =:= NothingTpe) { isValid = false; c.error(clause.pos, "must not convert to Nothing") }
              if (!(tpe <:< typeOf[scala.reflect.core.Tree])) { isValid = false; c.error(clause.pos, s"must only convert to Palladium trees, found ${precisetpe(body)}") }
              val components = extractIntersections(tpe)
              components.foreach(component => {
                val isLeaf = component.typeSymbol.annotations.exists(_.tree.tpe.typeSymbol == LeafAnnotation)
                if (!isLeaf) { isValid = false; c.error(clause.pos, s"must only convert to @ast classes or intersections thereof, found $tpe") }
              })
            }
          })
          isValid
        }
        def validateExhaustiveInputs(): Boolean = {
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
          if (unmatched.nonEmpty) c.error(c.enclosingPosition, "@converter is not exhaustive in its inputs; missing: " + unmatched)
          unmatched.isEmpty
        }
        def validateExhaustiveOutputs(): Boolean = {
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
          if (unmatched.nonEmpty) c.error(c.enclosingPosition, "@converter is not exhaustive in its outputs; missing: " + unmatched)
          unmatched.isEmpty
        }
        // val tups = clauses.map{ case CaseDef(pat, _, body) => (pat.tpe.toString.replace("HostContext.this.", ""), cleanLub(precisetpe(body) :: Nil).toString.replace("scala.reflect.core.", "p."), precisetpe(body).toString.replace("scala.reflect.core.", "p.")) }
        // val max1 = tups.map(_._1.length).max
        // val max2 = tups.map(_._2.length).max
        // tups.foreach{ case (f1, f2, f3) => println(f1 + (" " * (max1 - f1.length + 5)) + f2 + (" " * (max2 - f2.length + 5)) + f3) }
        // ???
        val isValid = List(validateAllowedInputs(), validateAllowedOutputs(), validateExhaustiveInputs(), validateExhaustiveOutputs()).forall(Predef.identity)
        if (isValid) {
          val nontrivialClauses = clauses.filter(clause => clause.body.symbol != Scalareflect_unreachable && clause.body.symbol != Predef_???)
          val ins = nontrivialClauses.map(_.pat.tpe)
          val underivedOuts = nontrivialClauses.map(_.body).map(body => if (body.symbol != Convert_derive) precisetpe(body) else NoType)
          val outs = nontrivialClauses.map({ case CaseDef(pat, _, body) =>
            if (body.symbol != Convert_derive) precisetpe(body)
            else lub(ins.zip(underivedOuts).collect{ case (in, out) if (in <:< pat.tpe) && (out != NoType) => out })
          })
          val methods = dummy.symbol.annotations.head.scalaArgs.map({
            case Literal(Constant(s: String)) =>
              val methodSym = wrapper.symbol.info.member(TermName(s)).orElse(c.abort(c.enclosingPosition, s"something went wrong: can't resolve $s in $wrapper"))
              q"$wrapper.$methodSym".duplicate
          })
          val deriveds = nontrivialClauses.map(_.body.symbol == Convert_derive)
          if (ins.length != outs.length || outs.length != methods.length || methods.length != deriveds.length) c.abort(c.enclosingPosition, s"something went wrong: can't create converters from ${ins.length}, ${outs.length}, ${methods.length} and ${deriveds.length}")
          ins.zip(outs).zip(methods).zip(deriveds).map{ case (((in, out), method), derived) => SharedConverter(in, out, method, derived) }
        } else {
          Nil
        }
      }
      val target = wrapper.symbol.asModule.moduleClass.orElse(c.abort(c.enclosingPosition, s"something went wrong: unexpected wrapper $wrapper"))
      val converters = target.name.toString match {
        case "toPalladium" => toPalladiumConverters
        case _ => c.abort(c.enclosingPosition, "unknown target: " + target.name)
      }
      // val tups = converters.map{ case SharedConverter(in: Type, out: Type, method: Tree, derived) => ((if (derived) "*" else "") + in.toString.replace("HostContext.this.", ""), cleanLub(out :: Nil).toString.replace("scala.reflect.core.", "p."), out.toString.replace("scala.reflect.core.", "p."), method) }
      // val max1 = tups.map(_._1.length).max
      // val max2 = tups.map(_._2.length).max
      // val max3 = tups.map(_._3.length).max
      // tups.foreach{ case (f1, f2, f3, f4) => println(f1 + (" " * (max1 - f1.length + 5)) + f2 + (" " * (max2 - f2.length + 5)) + f3 + (" " * (max3 - f3.length + 5)) + f4) }
      // ???
      target.updateAttachment(ComputedConvertersAttachment(converters))
      // TODO: also persist the converters in an annotation on target in order to support separate compilation
      q""
    }
    def loadConverters(): List[Converter] = {
      def loop(sym: Symbol): List[Converter] = {
        if (sym == NoSymbol) Nil
        else {
          val att = sym.attachments.get[ComputedConvertersAttachment]
          val result = att.map(_.converters.map(cvtr => Converter(cvtr.in.asInstanceOf[Type], cvtr.out.asInstanceOf[Type], cvtr.method.asInstanceOf[Tree], cvtr.derived)))
          val next = List(sym.owner) ++ (if (sym.isModule) List(sym.asModule.moduleClass) else Nil)
          result.getOrElse(next.flatMap(loop))
        }
      }
      loop(enclosingOwner)
    }
    def convert(x: Tree, in: Type, out: Type, alsoLookIntoDerived: Boolean, allowDowncasts: Boolean): Tree = {
      def fail(reason: String) = { c.error(x.pos, s"can't derive a converter from $in to $out because $reason"); gen.mkAttributedRef(Predef_???).setType(NothingTpe) }
      if (in.baseClasses.contains(SeqClass)) {
        if (!out.baseClasses.contains(SeqClass)) fail(s"of a collection rank mismatch")
        else {
          val in1 = in.baseType(SeqClass).typeArgs.head
          val out1 = out.baseType(SeqClass).typeArgs.head
          val param = c.freshName(TermName("x"))
          val result1 = convert(atPos(x.pos)(q"$param"), in1, out1, alsoLookIntoDerived, allowDowncasts)
          q"$x.map(($param: ${tq""}) => $result1)"
        }
      } else {
        val converters = loadConverters()
        def matchesIn(c: Converter) = (in <:< c.in) || (allowDowncasts && (c.in <:< in))
        def matchesOut(c: Converter) = (c.out <:< out) || (allowDowncasts && (out <:< c.out))
        var matching = converters.filter(c => !c.derived || alsoLookIntoDerived).filter(c => matchesIn(c) && matchesOut(c))
        matching = matching.filter(c1 => !matching.exists(c2 => c1 != c2 && !(c1.in =:= c2.in) && (c1.in <:< c2.in)))
        matching match {
          case Nil => fail(s"no suitable patterns were found");
          case Converter(_, _, method, _) :: Nil => q"$method($x)"
          case matching =>
            if (matching.map(_.in).length > matching.map(_.in).distinct.length) {
              val (ambin, ambout) = matching.groupBy(_.in).toList.sortBy(_._1.toString).filter(_._2.length > 1).head
              fail(s"$ambin <:< $in is ambiguous between ${ambout.map(_.out)}")
            } else {
              val cases = matching.map(c => cq"in: ${c.in} => ${c.method}(in)")
              q"""
                _root_.org.scalareflect.convert.internal.connectConverters($x match {
                  case null => sys.error("error converting from " + ${in.toString} + " to " + ${cleanLub(matching.map(_.out)).toString} + ": unexpected input null")
                  case ..$cases
                  case in => sys.error("error converting from " + ${in.toString} + " to " + ${cleanLub(matching.map(_.out)).toString} + ": unexpected input " + in.getClass.toString + ": " + in)
                })
              """
            }
        }
      }
    }
    def connectConverters(x: Tree): Tree = {
      val converters = loadConverters()
      if (converters.isEmpty) {
        if (c.hasErrors) x
        else c.abort(c.enclosingPosition, "something went wrong: can't obtain ComputedConvertersAttachment")
      } else if (x.exists(_.symbol == Convert_derive)) {
        val q"{ ..$_; in match { case ..$clauses } }" = x
        if (clauses.length != 1) c.abort(c.enclosingPosition, "can't derive a converter encompassing multiple clauses")
        val in = atPos(x.pos)(c.typecheck(Ident(TermName("in"))))
        convert(in, in.tpe, WildcardType, alsoLookIntoDerived = false, allowDowncasts = true)
      } else {
        object transformer {
          var pt: Type = WildcardType
          def refinePt(upperbound: Type): Unit = {
            if (pt != WildcardType) pt = cleanLub(List(pt, upperbound))
            else if (upperbound != NothingTpe) pt = upperbound
            else pt = WildcardType
          }
          def transform(tree: Tree): Tree = typingTransform(tree)((tree, api) => {
            def connect(convertee: Tree, force: Boolean): Tree = {
              println(convertee.tpe.widen + " -> " + pt + (if (force) " !!!" else ""))
              if (force) ???
              convert(convertee, convertee.tpe.widen, pt, alsoLookIntoDerived = true, allowDowncasts = force)
              gen.mkAttributedRef(Predef_???).setType(NothingTpe)
            }
            def transformApplyRememberingPts(app: Apply): Apply = {
              treeCopy.Apply(app, api.recur(app.fun), app.args.zipWithIndex.map({ case (arg, i) =>
                val params = app.fun.tpe.paramss.head
                def isVararg = i >= params.length - 1 && params.last.info.typeSymbol == RepeatedParamClass
                def unVararg(tpe: Type) = tpe.typeArgs.head
                pt = if (isVararg) unVararg(params.last.info) else params(i).info
                val result = api.recur(arg)
                pt = WildcardType
                result
              }))
            }
            // TODO: think whether we can rebind polymorphic usages of cvt in a generic fashion
            // without hardcoding to every type constructor we're going to use
            tree match {
              case q"$_.List.apply[$_](${Cvt(convertee, cvtPt, force)})" =>
                pt = if (pt != WildcardType) pt.typeArgs.head else pt
                refinePt(cvtPt)
                mkAttributedApply(List_apply, connect(convertee, force))
              case q"$_.Some.apply[$_](${Cvt(convertee, cvtPt, force)})" =>
                pt = if (pt != WildcardType) pt.typeArgs.head else pt
                refinePt(cvtPt)
                mkAttributedApply(Some_apply, connect(convertee, force))
              case Cvt(convertee, cvtPt, force) =>
                refinePt(cvtPt)
                connect(convertee, force)
              case app: Apply =>
                transformApplyRememberingPts(app)
              case _ =>
                pt = WildcardType
                api.default(tree)
            }
          })
        }
        transformer.transform(x).setType(cleanLub(x.tpe :: Nil))
      }
    }
    // NOTE: postprocessing is not mandatory, but it's sort of necessary to simplify types
    // due to the fact that we have `type ThisType` in every root, branch and leaf node
    // lubs of node types can have really baroque ThisType refinements :)
    def cleanLub(tpes: List[Type]): Type = {
      object Scope { def unapplySeq(scope: Scope): Some[Seq[Symbol]] = Some(scope.toList) }
      object ThisType { def unapply(sym: Symbol): Boolean = sym.name == TypeName("ThisType") }
      lub(tpes) match {
        case RefinedType(underlying :: Nil, Scope(ThisType())) => underlying
        case RefinedType(parents, Scope(ThisType())) => cleanLub(parents)
        case lub => lub
      }
    }
    def extractIntersections(tpe: Type): List[Type] = {
      object Scope { def unapplySeq(scope: Scope): Some[Seq[Symbol]] = Some(scope.toList) }
      object ThisType { def unapply(sym: Symbol): Option[Symbol] = if (sym.name == TypeName("ThisType")) Some(sym) else None }
      tpe.dealias match {
        case RefinedType(_, Scope(ThisType(sym))) =>
          sym.info match {
            case TypeBounds(RefinedType(parents, Scope()), _) => parents
            case TypeBounds(parent, _) => List(parent)
            case _ => unreachable
          }
        case tpe =>
          List(tpe)
      }
    }
    def mkAttributedApply(m: MethodSymbol, arg: Tree): Apply = {
      val owner = if (m.owner.isModuleClass) m.owner.asClass.module else m.owner
      val pre = gen.mkAttributedRef(owner)
      val polysel = gen.mkAttributedSelect(pre, m)
      val sel = TypeApply(polysel, List(TypeTree(arg.tpe))).setType(appliedType(polysel.tpe, arg.tpe))
      Apply(sel, List(arg)).setType(sel.tpe.finalResultType)
    }
  }
}
