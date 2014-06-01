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
      case class Instance(in: Tree, out: Tree, clauses: List[Tree], isStub: Boolean) {
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
          val isStub = body match { case q"unreachable" => true; case q"???" => true; case _ => false }
          val i = instances.indexWhere{ case Instance(iin, iout, _, _) => in.toString == iin.toString && out.toString == iout.toString }
          if (i == -1) instances += Instance(in, out, List(clause), isStub)
          else instances(i) = Instance(in, out, instances(i).clauses :+ clause, isStub)
      })
      val precomputeParts = instances.map({
        case Instance(_, _, Nil, _) => unreachable
        case Instance(_, _, clause :: Nil, _) => clause
        case Instance(in, _, clauses, _) => cq"in: $in => in match { case ..$clauses }"
      })
      val precompute = q"""
        _root_.org.scalareflect.convert.ConvertInternal.precompute[$wrapper.type, ${constantType(Constant(instances.filter(!_.isStub).length))}]({
          def $name(in: Any): Any = {
            ..$prelude
            in match { case ..$precomputeParts }
          }
          ()
        })
      """
      val instanceDecls = instances.filter(!_.isStub).map({ instance =>
        q"""
          implicit val ${instance.decl} = ${companion.toTypeName}.this.apply((in: ${instance.in}) => {
            ${wrapper.toTypeName}.this.${instance.impl}(in)
          })
        """
      })
      val instanceImpls = instances.filter(!_.isStub).map({ case instance =>
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
  def precompute[T, U](x: Any): Unit = macro ConvertInternalMacro.precompute[T, U]
}
class ConvertInternalMacro(val c: Context) {
  import c.universe._
  def materialize[In: WeakTypeTag, Out: WeakTypeTag]: Tree = {
    ???
  }
  def precompute[T: WeakTypeTag, U: WeakTypeTag](x: Tree): Tree = {
    import c.internal._, decorators._
    val q"{ def $_(in: $_): $_ = { ..$prelude; in match { case ..$clauses } }; () }" = x
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
      relevantClauses.map(_.body).map(precisetpe)
    }
    val target = weakTypeOf[T].typeSymbol
    val tpes = target.name.toString match {
      case "toPalladium" => toPalladiumTpes
      case _ => c.abort(c.enclosingPosition, "unknown target: " + target.name)
    }
    val expectedTpesLength = weakTypeOf[U].asInstanceOf[ConstantType].value.value.asInstanceOf[Int]
    if (tpes.length != expectedTpesLength) c.abort(c.enclosingPosition, "something went wrong: expected $expectedTpesLength tpes, actual ${tpes.length} tpes")
    target.updateAttachment(PrecomputeAttachment(tpes))
    q""
  }
}
