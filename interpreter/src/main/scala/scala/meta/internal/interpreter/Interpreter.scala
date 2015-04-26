package scala.meta.internal
package interpreter

import scala.util.matching._
import scala.meta._
import scala.meta.semantic._
import scala.meta.internal.{ ast => m }
import scala.meta.dialects.Scala211

import scala.collection.immutable.ListMap
import scala.meta.internal.interpreter.Environment._
import org.scalameta._
import org.scalameta.invariants._

object Interpreter {

  def eval(term: Term)(implicit c: Context): Any = eval(term, Map[Term.Name, Any]())

  def eval(term: Term, env0: Map[Term.Name, Any])(implicit c: Context): Any = {
    def typeOf(v: Any): Type = {
      if (v == null) return t"Null"
      def loadModule(root: Term.Name, parts: List[String]): Term.Name = parts match {
        case Nil => 
          root
        case name :: rest => 
          val child = root.members.filter(m => (m.isPackage || m.isObject) && m.name.toString == name).head.asInstanceOf[Member.Term]
          loadModule(child.name, rest)
      }
      val init :+ last = v.getClass.getName.split(Array('.', '$')).toList
      loadModule(q"_root_", init).classes(last).name
    }
    val frame0 = ListMap(env0.map{ case (k, v) => (k.asInstanceOf[TName], Object(v, typeOf(v))) }.toList: _*)
    val env1 = Env(List(frame0), ListMap[m.Term.Name, Object]())
    val (Object(v, tpe, fields), env2) = eval(term, env1)
    v
  }

  def eval(term: Tree, env: Env)(implicit c: Context): (Object, Env) = {
    term match {
      // Quasiquotes (this should not exist)
      case m.Term.Apply(m.Term.Name("Unlift"), List(arg)) =>
        eval(arg, env)
      // Quasiquotes (this should not exist)
      case m.Term.Apply(m.Term.Name("Lift"), List(arg)) =>
        val (v, env1) = eval(arg, env)
        if (!v.ref.isInstanceOf[m.Tree] && !v.ref.isInstanceOf[List[_]]) {
          (Object(m.Lit.String(v.as[String]), t"Int"), env1)
        } else (v, env1)

      case m.Term.Block(stats) =>
        stats.foldLeft((Object((), t"Unit"), env))((res, stat) => eval(stat, res._2))

      case nme @ m.Term.Name(_) =>
        (env.lookup(nme), env)

      // TODO(non-reflective): dispatch to pattern matching in an elegant way
      case m.Defn.Val(mods, List(m.Pat.Var.Term(name)), tpe, body) =>
        val (res, newEnv) = eval(body, env)
        // TODO(non-reflective): depends of the context (method body or template)
        (Object((), t"Unit"), newEnv.push(name, res))

      case m.Defn.Var(mods, List(m.Pat.Var.Term(name)), tpe, body) =>
        val (res, newEnv) = body.map(eval(_, env)).getOrElse((Object(null, t"Null"), env))
        // TODO(non-reflective): depends of the context (method body or template)
        (Object((), t"Unit"), newEnv.push(name, res))

      case m.Term.Assign(varNme, body) =>
        val (res, env1) = eval(body, env)
        (Object((), t"Unit"), env1.push(varNme.asInstanceOf[m.Term.Name], res))

      case m.Lit.Byte(v) => (Object(v, t"Byte"), env)
      case m.Lit.Short(v) => (Object(v, t"Short"), env)
      case m.Lit.Int(v) => (Object(v, t"Int"), env)
      case m.Lit.Long(v) => (Object(v, t"Long"), env)
      case m.Lit.Float(v) => (Object(v, t"Float"), env)
      case m.Lit.Double(v) => (Object(v, t"Double"), env)
      case m.Lit.String(v) => (Object(v, t"String"), env)
      case m.Lit.Bool(v) => (Object(v, t"Boolean"), env)
      case m.Lit.Unit() => (Object((), t"Unit"), env)

      case m.Term.Tuple(terms) => // TODO all sizes
        val (res, env1) = evalSeq(terms, env)
        res match {
          case e1 :: e2 :: Nil => (Object((e1.ref, e2.ref), t"(${e1.tpe}, ${e2.tpe})"), env1)
          case e1 :: e2 :: e3 :: Nil => (Object((e1.ref, e2.ref, e3.ref), t"(${e1.tpe}, ${e2.tpe}, ${e3.tpe})"), env1)
          case _ => unreachable(debug(res))
        }

      case m.Term.Select((lhs: m.Term.Name), op) if lhs.isPackage =>
        (env.lookup(op), env)

      case trm @ m.Term.Select(lhs, op) if op.isObject =>
        (env.lookup(op), env)

      case m.Term.Select(lhs, op) =>
        val (lhsV, env1) = eval(lhs, env)
        val method = lhs.tpe.members.filter(_.name == op).head
        methodCall(method, lhsV, Seq(), env1)

      case m.Term.ApplyUnary(nme, lhs) =>
        val method = lhs.tpe.members.filter(_.name == nme).head
        val (lhsV, newEnv) = eval(lhs, env)
        methodCall(method, lhsV, Seq(), newEnv)

      case m.Term.ApplyInfix(lhs, op, _, args) =>
        val method = lhs.tpe.members.filter(_.name == op).head
        val (lhsV, newEnv) = eval(lhs, env)
        val (rhsVs, callEnv) = evalSeq(args, env)
        methodCall(method, lhsV, rhsVs, callEnv)

      case m.Term.Apply(fun @ m.Term.Select(lhs, rhs), args) if fun.isDef =>
        val (lhsV, newEnv) = eval(lhs, env)
        val method = lhs.tpe.members.filter(_.name == rhs.name).head
        val (rhsVs, callEnv) = evalSeq(args, env)
        methodCall(method, lhsV, rhsVs, callEnv)

      case m.Term.Apply(lhs@m.Term.Name("abort"), List(arg)) if lhs.isDef =>
        val (Object(msg: String, _, _), env1) = eval(arg, env)
        env.context.asInstanceOf[scala.meta.macros.Context].abort(msg)
        (Object((), t"Nothing"), env1)

      case m.Term.Apply(lhs@m.Term.Name("println"), List(arg)) if lhs.isDef =>
        val (Object(value, _, _), env1) = eval(arg, env)
        println(value.toString)
        (Object((), t"Unit"), env1)

      case m.Term.Apply(lhs: m.Term.Name, args) if lhs.isDef =>
        val res = eval(lhs, env)
        res match {
          case (Object(lhsV: Function1[Any, Any] with FunctionEnv, tpe, _), newEnv) =>
            val (rhsVs, callEnv) = evalSeq(args, env)
            val res = lhsV.apply(rhsVs(0).ref)
            (Object(res, t"Any"), lhsV.functionEnv)
          case (Object(lhsV: Function2[Any, Any, Any] with FunctionEnv, tpe, _), newEnv) =>
            val (rhsVs, callEnv) = evalSeq(args, env)
            val res = lhsV.apply(rhsVs(0).ref, rhsVs(1).ref)
            (Object(res, t"Any"), lhsV.functionEnv)
          case (Object(lhsV: Function3[Any, Any, Any, Any] with FunctionEnv, tpe, _), newEnv) =>
            val (rhsVs, callEnv) = evalSeq(args, env)
            val res = lhsV.apply(rhsVs(0).ref, rhsVs(1).ref, rhsVs(2).ref)
            (Object(res, t"Any"), lhsV.functionEnv)
        }

      // TODO: with quasiquotes this should be easy and more general
      case m.Term.Apply(m.Term.Apply(m.Term.Select(lhs, rhs), args1), args2) =>
        val (lhsV, env1) = eval(lhs, env)
        val method = lhs.tpe.members.filter(_.name == rhs.name).head
        val (rhsVs1, env2) = evalSeq(args1, env1)
        val (rhsVs2, env3) = evalSeq(args2, env2)
        methodCall(method, lhsV, rhsVs1 ++ rhsVs2, env3)

      case m.Term.Apply(m.Term.Apply(name: m.Term.Name, args1), args2) =>
        val defn @ m.Defn.Def(_, _, _, paramss, _, body) = name.defn
        val (rhsVs1, env1) = evalSeq(args1, env)
        val (rhsVs2, env2) = evalSeq(args2, env1)
        val argss = List(rhsVs1, rhsVs2)
        val env3 = (argss zip paramss).foldLeft(env2) { (agg, p) =>
          (p._1 zip p._2).foldLeft(agg) { (agg1, pv) =>
            agg1.push(pv._2.name.asInstanceOf[m.Term.Name], pv._1)
          }
        }
        eval(body, env3)

      case m.Term.Apply(lhs, args) =>
        val method = lhs.tpe.defs("apply")
        val (lhsV, newEnv) = eval(lhs, env)
        val (rhsVs, callEnv) = evalSeq(args, env)
        methodCall(method, lhsV, rhsVs, callEnv)

      // TODO generalize
      case m.Term.ApplyType(m.Term.Select(lhs, m.Term.Name("asInstanceOf")), tpes) =>
        val (lhsV, env1) = eval(lhs, env)
        // TODO does not work
        // if (lhs.tpe.dealias <:< tpes.head.dealias) (lhsV, env1)
        if (true) (lhsV, env1)
        else throw new ClassCastException(s"!${lhs.tpe.dealias} <:< ${tpes.head}")

      case m.Term.ApplyType(m.Term.Select(tree, m.Term.Name("show")), List(printer)) =>
        val (Object(treeV: Tree, _, _), env1) = eval(tree, env)
        val message = {
          if (printer.toString == "Code") treeV.show[Code]
          else if (printer.toString == "Raw") treeV.show[Raw]
          else if (printer.toString == "Semantics") treeV.show[Semantics]
          else if (printer.toString == "Positions") treeV.show[Positions]
          else unreachable(debug(tree, printer))
        }
        (Object(message, t"String"), env1)

      case m.Term.Arg.Named(nme, v) => // TODO keep the function in the context
        eval(v, env)

      case m.Term.If(cond, thn, elze) =>
        val (Object(cV: Boolean, _, _), cEnv: Env) = eval(cond, env)
        if (cV) eval(thn, cEnv)
        else eval(elze, cEnv)

      case func @ m.Defn.Def(mods, nme, _, paramss, retTp, body) =>
        def evalFunc(metaprogram: Tree, argss: Seq[Seq[Any]], env: Env): (Object, Env) = {
          val defn @ m.Defn.Def(mods, nme, _, paramss, _, body) = metaprogram
          val newEnv = (argss zip paramss).foldLeft(env) { (agg, p) =>
            (p._1 zip p._2).foldLeft(agg) { (agg1, pv) =>
              agg1.push(pv._2.name.asInstanceOf[m.Term.Name], Object(pv._1, pv._2.tpe))
            }
          }
          eval(body, newEnv)
        }

        // TODO multiple-functions
        paramss.flatten match {
          case p1 :: Nil =>
            val funcObj = Object(new Function1[Any, Any] with FunctionEnv {
              var functionEnv: Env = env // TODO fix
              def apply(x: Any) = {
                val (res, env1) = evalFunc(func, Seq(Seq(x)), functionEnv)
                functionEnv = env1
                res.ref
              }
            }, t"${p1.tpe} => Any") // TODO cant get the type if not indicated

            (funcObj, env.push(nme, funcObj))
          case p1 :: p2 :: p3 :: Nil =>
            val funcObj = Object(new Function3[Any, Any, Any, Any] with FunctionEnv {
              var functionEnv: Env = env // TODO fix
              def apply(x: Any, y: Any, z: Any) = {
                val (res, env1) = evalFunc(func, Seq(Seq(x, y, z)), functionEnv)
                functionEnv = env1
                res.ref
              }
            }, t"(${p1.tpe}, ${p2.tpe}, ${p3.tpe}) => Any")

            (funcObj, env.push(nme, funcObj))
        }
      case m.Term.Ascribe(v, tp) =>
        eval(v, env)

      // TODO specialization
      case m.Term.Function(params, bodyTerm) =>
        // TODO all functions!
        // TODO store the returning env!
        // TODO proper type.
        params match {
          case p1 :: Nil =>
            (Object(new Function1[Any, Any] with FunctionEnv {
              var functionEnv: Env = env // TODO fix
              def apply(x: Any) = {
                val env1 = env.push(p1.name.asInstanceOf[m.Term.Name], Object(x, p1.tpe))
                val (res, resEnv) = eval(bodyTerm, env1)
                functionEnv = resEnv
                res.ref
              }
            }, t"Any => Any"), env)
          case p1 :: p2 :: Nil =>
            (Object(new Function2[Any, Any, Any] with FunctionEnv {
              var functionEnv: Env = env // TODO fix
              def apply(x: Any, y: Any) = {
                val env1 = env.push(p1.name.asInstanceOf[m.Term.Name], Object(x, p1.tpe))
                val env2 = env1.push(p2.name.asInstanceOf[m.Term.Name], Object(y, p2.tpe))
                val (res, resEnv) = eval(bodyTerm, env2)
                functionEnv = resEnv
                res.ref
              }
            }, t"(Any, Any) => Any"), env)
        }

      // String interpolators
      case m.Term.Interpolate(m.Term.Name("s"), strings, terms) =>
        val (evalTerms, env1) = evalSeq(terms, env)
        val (evalStrings, env2) = evalSeq(strings, env1)
        val res = evalStrings.map(_.ref).zipAll(evalTerms.map(_.ref.toString), "", "").foldLeft("")((res, s) => res + s._1 + s._2)
        (Object(res, t"String"), env2)

      // Pattern matching
      case m.Term.Match(lhs, cases) =>
        val (lhsV, env1) = eval(lhs, env)
        var env2 = env1
        cases.collectFirst({
          case m.Case(pattern, guard, block) if ({
            { val (res, env3) = evalPattern(lhsV, pattern, env2); env2 = env3; res.as[Boolean] } &&
              (guard.isEmpty || { val (res, env3) = evalGuard(lhsV, guard.get, env2); env2 = env3; res.as[Boolean] })
          }) =>
            eval(block, env2)
        }).getOrElse(throw new MatchError(lhsV.ref, lhsV.tpe)) // TODO this needs to be an evaluated exception

      case _ => unreachable(debug(term, term.show[Semantics]))
    }
  }

  def evalPattern(lhs: Object, pattern: m.Pat, env: Env)(implicit c: Context): (Object, Env) = pattern match {
    case m.Pat.Typed(x, tp) =>
      (Object(lhs.tpe <:< tp.tpe, t"Boolean"), extendEnv(x, lhs, env))
    case m.Pat.Wildcard() =>
      (Object(true, t"Boolean"), env)
    case _ => unreachable(debug(pattern, pattern.show[Semantics]))
  }

  def extendEnv(v: m.Tree, lhs: Object, env: Env)(implicit c: Context): Env = v match {
    case m.Pat.Var.Term(name) => env.push(name, lhs)
    case _ => unreachable(debug(v, v.show[Semantics]))
  }

  def evalGuard(lhs: Object, guard: m.Term, env: Env)(implicit x: Context): (Object, Env) = ???

  def evalSeq[T <: Tree](terms: Seq[T], env: Env)(implicit c: Context): (Seq[Object], Env) =
    terms.foldLeft((Seq[Object](), env)) { (acc, arg) =>
      val (res, newEnv) = eval(arg, acc._2)
      (acc._1 :+ res, newEnv)
    }

  def methodCall(member: Member, lhs: Object, rhs: Seq[Object], env: Env)(implicit c: Context): (Object, Env) = {
    member match {
      // abstract method
      case m @ m.Decl.Def(mods, _, _, paramss, _) => // interface
        methodCallByMods(mods, paramss, lhs, rhs, env)
      case m.Defn.Def(mods, _, _, paramss, _, m.Term.Name("???")) =>
        methodCallByMods(mods, paramss, lhs, rhs, env)
      case m @ m.Pat.Var.Term(nme) =>
        methodCallByMods(member.mods.asInstanceOf[scala.collection.Seq[scala.meta.internal.ast.Mod]], Nil, lhs, rhs, env)
      case _ => unreachable(debug(member, member.show[Semantics]))
    }
  }

  def methodCallByMods(mods: Seq[m.Mod], params: Seq[Seq[m.Term.Param]], lhs: Object, rhs: Seq[Object], env: Env)(implicit c: Context) = {
    val jvmMethod: String = mods.collect({ case m.Mod.Ffi(s) => s }).head
    val rgx = """(.*)\((.*), (.*), (.*)""".r
    jvmMethod.substring(0, jvmMethod.length - 1) match {
      case rgx("scalaIntrinsic", lhsJTp, nme, argsRetJTp) =>
        ScalaEmulator.emulate(List(lhsJTp, nme, argsRetJTp), lhs +: rhs, env)
      case rgx("jvmMethod", "Ljava/lang/Object;", "$plus", "(Ljava/lang/String;)Ljava/lang/String;") => // ARGH!
        methodCallByJavaSignature(params, Object(lhs.ref.toString, t"String"), rhs, "Ljava/lang/String;", "concat", "(Ljava/lang/String;)Ljava/lang/String", env)
      case rgx("jvmMethod", lhsJTp, nme, argsRetJTp) =>
        methodCallByJavaSignature(params, lhs, rhs, lhsJTp, nme, argsRetJTp, env)
      case _ => unreachable(debug(jvmMethod.substring(0, jvmMethod.length - 1)))
    }
  }

  def methodCallByJavaSignature(
    params: Seq[Seq[m.Term.Param]], lhs: Object, rhs: Seq[Object],
    lhsJTp: String, nme: String, argsRetJTp: String, env: Env)(implicit c: Context) = {
    val paramsRegex = """\((.*)\).*""".r
    val paramsRegex(jvmParams) = argsRetJTp
    val types = jvmParams.replaceAll("(L.*?;|I|B)", "$1,").split(",").toList.filterNot(_.trim.isEmpty).map(Utils.jvmTypeToClass)
    val jMethod = Utils.jvmTypeToClass(lhsJTp).getMethod(nme, types: _*)

    val repeated = params.exists(_.exists {
      case m.Term.Param(_, _, Some(m.Type.Arg.Repeated(_)), _) => true
      case _ => false
    })
    val (vLHS, vRHS) = (lhs.ref, rhs.map(_.ref))
    val args = if (repeated) // pack trailing arguments into a seq
      (vRHS.take(types.length - 1) :+ Seq(vRHS.drop(types.length - 1): _*)).toSeq
    else vRHS
    val jvmArgs = args.toSeq.asInstanceOf[Seq[AnyRef]]
    try {
      val (eLHS, eArgs: Seq[AnyRef]) = (vLHS, lhsJTp, nme, argsRetJTp) match {
        // Std. Library
        // List/Seq
        case (vLHS, "Lscala/collection/SeqLike;", "$colon$plus", "(Ljava/lang/Object;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;") =>
          (vLHS, jvmArgs ++ Seq(Seq.canBuildFrom))
        case (vLHS, "Lscala/collection/immutable/List;", "map", "(Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;") =>
          (vLHS, jvmArgs ++ Seq(List.canBuildFrom))
        case (vLHS, "Lscala/collection/TraversableLike;", "map", "(Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;") =>
          (vLHS, jvmArgs ++ Seq(Traversable.canBuildFrom))

        // Option
        case (vLHS, "Lscala/Option;", "orElse", "(Lscala/Function0;)Lscala/Option;") =>
          (vLHS, Seq(() => jvmArgs.head))
        case (vLHS, "Lscala/Option;", "getOrElse", "(Lscala/Function0;)Ljava/lang/Object;") =>
          (vLHS, Seq(() => jvmArgs.head))

        // Constructors
        case (vLHS, "Lscala/meta/internal/ast/Lit$String$;", "apply", "(Ljava/lang/String;Lscala/meta/Origin;)Lscala/meta/internal/ast/Lit$String;") =>
          (vLHS, jvmArgs ++ Seq(scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Term$Name$;", "apply", "(Ljava/lang/String;Lscala/meta/internal/hygiene/Denotation;Lscala/meta/internal/hygiene/Sigma;Lscala/meta/Origin;)Lscala/meta/internal/ast/Term$Name;") =>
          (vLHS, jvmArgs ++ Seq(scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Term$ApplyInfix$;", "apply", "(Lscala/meta/internal/ast/Term;Lscala/meta/internal/ast/Term$Name;Lscala/collection/immutable/Seq;Lscala/collection/immutable/Seq;Lscala/meta/Origin;)Lscala/meta/internal/ast/Term$ApplyInfix;") =>
          (vLHS, jvmArgs ++ Seq(scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Term$Block$;", "apply", "(Lscala/collection/immutable/Seq;Lscala/meta/Origin;)Lscala/meta/internal/ast/Term$Block;") =>
          (vLHS, jvmArgs ++ Seq(scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Case$;", "apply", "(Lscala/meta/internal/ast/Pat;Lscala/Option;Lscala/meta/internal/ast/Term$Block;Lscala/meta/Origin;)Lscala/meta/internal/ast/Case;") =>
          (vLHS, jvmArgs ++ Seq(scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Term$Match$;", "apply", "(Lscala/meta/internal/ast/Term;Lscala/collection/immutable/Seq;Lscala/meta/Origin;)Lscala/meta/internal/ast/Term$Match;") =>
          (vLHS, jvmArgs ++ Seq(scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Mod$Implicit$;", "apply", "(Lscala/meta/Origin;)Lscala/meta/internal/ast/Mod$Implicit;") =>
          (vLHS, jvmArgs ++ Seq(scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Ctor$Ref$Name$;", "apply", "(Ljava/lang/String;Lscala/meta/internal/hygiene/Denotation;Lscala/meta/internal/hygiene/Sigma;Lscala/meta/Origin;)Lscala/meta/internal/ast/Ctor$Ref$Name;") =>
          val addArgs =
            if (jvmArgs.size == 1) Seq(scala.meta.internal.hygiene.Denotation.Zero, scala.meta.internal.hygiene.Sigma.Naive, scala.meta.Origin.None)
            else Seq[AnyRef](scala.meta.Origin.None)
          (vLHS, jvmArgs ++ addArgs)
        case (vLHS, "Lscala/meta/internal/ast/Ctor$Primary$;", "apply", "(Lscala/collection/immutable/Seq;Lscala/meta/internal/ast/Ctor$Ref$Name;Lscala/collection/immutable/Seq;Lscala/meta/Origin;)Lscala/meta/internal/ast/Ctor$Primary;") =>
          (vLHS, jvmArgs ++ Seq(scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Name$Anonymous$;", "apply", "(Lscala/meta/internal/hygiene/Denotation;Lscala/meta/internal/hygiene/Sigma;Lscala/meta/Origin;)Lscala/meta/internal/ast/Name$Anonymous;") =>
          (vLHS, jvmArgs ++ (if (jvmArgs.size == 0) Seq[AnyRef](scala.meta.internal.hygiene.Denotation.Zero, scala.meta.internal.hygiene.Sigma.Naive, scala.meta.Origin.None) else Seq[AnyRef](scala.meta.Origin.None)))
        case (vLHS, "Lscala/meta/internal/ast/Type$Name$;", "apply", "(Ljava/lang/String;Lscala/meta/internal/hygiene/Denotation;Lscala/meta/internal/hygiene/Sigma;Lscala/meta/Origin;)Lscala/meta/internal/ast/Type$Name;") =>
          (vLHS, jvmArgs ++ Seq[AnyRef](scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Defn$Def$;", "apply", "(Lscala/collection/immutable/Seq;Lscala/meta/internal/ast/Term$Name;Lscala/collection/immutable/Seq;Lscala/collection/immutable/Seq;Lscala/Option;Lscala/meta/internal/ast/Term;Lscala/meta/Origin;)Lscala/meta/internal/ast/Defn$Def;") =>
          (vLHS, jvmArgs ++ Seq[AnyRef](scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Defn$Object$;", "apply", "(Lscala/collection/immutable/Seq;Lscala/meta/internal/ast/Term$Name;Lscala/meta/internal/ast/Ctor$Primary;Lscala/meta/internal/ast/Template;Lscala/meta/Origin;)Lscala/meta/internal/ast/Defn$Object;") =>
          (vLHS, jvmArgs ++ Seq[AnyRef](scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Term$ApplyType$;", "apply", "(Lscala/meta/internal/ast/Term;Lscala/collection/immutable/Seq;Lscala/meta/Origin;)Lscala/meta/internal/ast/Term$ApplyType;") =>
          (vLHS, jvmArgs ++ Seq[AnyRef](scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Term$Param$;", "apply", "(Lscala/collection/immutable/Seq;Lscala/meta/internal/ast/Term$Param$Name;Lscala/Option;Lscala/Option;Lscala/meta/Origin;)Lscala/meta/internal/ast/Term$Param;") =>
          (vLHS, jvmArgs ++ Seq[AnyRef](scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Pat$Typed$;", "apply", "(Lscala/meta/internal/ast/Pat;Lscala/meta/internal/ast/Pat$Type;Lscala/meta/Origin;)Lscala/meta/internal/ast/Pat$Typed;") =>
          (vLHS, jvmArgs ++ Seq[AnyRef](scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Template$;", "apply", "(Lscala/collection/immutable/Seq;Lscala/collection/immutable/Seq;Lscala/meta/internal/ast/Term$Param;Lscala/Option;Lscala/meta/Origin;)Lscala/meta/internal/ast/Template;") =>
          (vLHS, jvmArgs ++ Seq[AnyRef](scala.meta.Origin.None))
        case (vLHS, "Lscala/meta/internal/ast/Term$New$;", "apply", "(Lscala/meta/internal/ast/Template;Lscala/meta/Origin;)Lscala/meta/internal/ast/Term$New;") =>
          (vLHS, jvmArgs ++ Seq[AnyRef](scala.meta.Origin.None))
        case (vLHS,"Lscala/meta/internal/ast/Term$Select$;","apply","(Lscala/meta/internal/ast/Term;Lscala/meta/internal/ast/Term$Name;Lscala/meta/Origin;)Lscala/meta/internal/ast/Term$Select;") =>
          (vLHS, jvmArgs ++ Seq[AnyRef](scala.meta.Origin.None))
        case (vLHS,"Lscala/meta/internal/ast/Term$Apply$;","apply","(Lscala/meta/internal/ast/Term;Lscala/collection/immutable/Seq;Lscala/meta/Origin;)Lscala/meta/internal/ast/Term$Apply;") =>
          (vLHS, jvmArgs ++ Seq[AnyRef](scala.meta.Origin.None))

        // XtensionSemanticMemberLike
        case (vLHS: Member, "Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "isFinal", "(Lscala/meta/semantic/Context;)Z") =>
          (XtensionSemanticMember(vLHS), jvmArgs ++ Seq(env.context))
        case (vLHS: Member, "Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "isClass", "(Lscala/meta/semantic/Context;)Z") =>
          (XtensionSemanticMember(vLHS), jvmArgs ++ Seq(env.context))
        case (vLHS: Member, "Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "isObject", "(Lscala/meta/semantic/Context;)Z") =>
          (XtensionSemanticMember(vLHS), jvmArgs ++ Seq(env.context))
        case (vLHS: Member, "Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "isCase", "(Lscala/meta/semantic/Context;)Z") =>
          (XtensionSemanticMember(vLHS), jvmArgs ++ Seq(env.context))
        case (vLHS: Member, "Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "isTrait", "(Lscala/meta/semantic/Context;)Z") =>
          (XtensionSemanticMember(vLHS), jvmArgs ++ Seq(env.context))
        case (vLHS: Member, "Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "isSealed", "(Lscala/meta/semantic/Context;)Z") =>
          (XtensionSemanticMember(vLHS), jvmArgs ++ Seq(env.context))
        case (vLHS: Member, "Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "name", "(Lscala/meta/semantic/Context;)Lscala/meta/Name;") =>
          (XtensionSemanticMember(vLHS), jvmArgs ++ Seq(env.context))
        case (vLHS: m.Member, "Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "parents", "(Lscala/meta/semantic/Context;)Lscala/collection/immutable/Seq;") =>
          (XtensionSemanticMember(vLHS), jvmArgs ++ Seq(env.context))
        case (vLHS: m.Member, "Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "children", "(Lscala/meta/semantic/Context;)Lscala/collection/immutable/Seq;") =>
          (XtensionSemanticMember(vLHS), jvmArgs ++ Seq(env.context))


        // SemanticScopeLike
        case (vLHS: Scope, "Lscala/meta/semantic/Api$XtensionSemanticScopeLike;", "ctor", "(Lscala/meta/semantic/Context;)Lscala/meta/Member$Term;") =>
          (XtensionSemanticScope(vLHS), jvmArgs ++ Seq(env.context))
        case (vLHS: Scope, "Lscala/meta/semantic/Api$XtensionSemanticScopeLike;", "tparams", "(Lscala/meta/semantic/Context;)Lscala/collection/immutable/Seq;") =>
          (XtensionSemanticScope(vLHS), jvmArgs ++ Seq(env.context))
        case (vLHS: Scope, "Lscala/meta/semantic/Api$XtensionSemanticScopeLike;", "params", "(Lscala/meta/semantic/Context;)Lscala/collection/immutable/Seq;") =>
          (XtensionSemanticScope(vLHS), jvmArgs ++ Seq(env.context))

        case (vLHS: m.Type.Ref, "Lscala/meta/semantic/Api$XtensionSemanticTypeRefDefn;", "defn", "(Lscala/meta/semantic/Context;)Lscala/meta/Member;") =>
          (XtensionSemanticTypeRefDefn(vLHS), jvmArgs ++ Seq(env.context))

        case (vLHS: m.Member.Term, "Lscala/meta/semantic/Api$XtensionSemanticTermMember;", "name", "(Lscala/meta/semantic/Context;)Lscala/meta/Term$Name;") =>
          (XtensionSemanticTermMember(vLHS), jvmArgs ++ Seq(env.context))

        case (vLHS: Term.Param,"Lscala/meta/semantic/Api$XtensionSemanticTermParam;","field","(Lscala/meta/semantic/Context;)Lscala/meta/Member$Term;") =>
          (XtensionSemanticTermParam(vLHS), jvmArgs ++ Seq(env.context))

        case (vLHS: Member,"Lscala/meta/semantic/Api$XtensionSemanticMemberTpe;","tpe","(Lscala/meta/semantic/Context;)Lscala/meta/Type;") =>
          (XtensionSemanticMemberTpe(vLHS), jvmArgs ++ Seq(env.context))

        case (vLHS: Type, "Lscala/meta/semantic/Api$XtensionTypeToPatType;", "pat", "(Lscala/meta/semantic/Context;)Lscala/meta/Pat$Type;") =>
          (XtensionTypeToPatType(vLHS), jvmArgs ++ Seq(env.context))

        case x => (vLHS, jvmArgs)
      }
      (Object(jMethod.invoke(eLHS, eArgs: _*), t"List[Int]"), env)
    } catch {
      case e: java.lang.reflect.InvocationTargetException =>
        throw e.getCause()
    }
  }
  // TODO: this class and its method shouldn't exist
  // the only reason for them to be here is that we don't have an API for desugaring yet
  // so we have to guess implicit arguments on our own
  implicit class EnvContext(env: Env) {
    def context(implicit c: Context): Context = {
      // TODO: why doesn't this work?
      // env.stack.flatten.find(_._2.tpe <:< t"Context").head._2.ref.asInstanceOf[Context]
      c
    }
  }
}

object Utils {
  def jvmTypeToClass(s: String): Class[_] = s match {
    case "I" => classOf[Int]
    case "Z" => classOf[Boolean]
    case _ =>
      Class.forName(s.replaceAll("/", ".").subSequence(1, s.length - 1).toString)
  }
}
