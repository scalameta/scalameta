package scala.meta.internal
package interpreter

import scala.util.matching._
import scala.meta._
import scala.meta.semantic._
import scala.meta.internal.{ ast => i }
import scala.meta.dialects.Scala211

import scala.collection.immutable.ListMap
import scala.meta.internal.interpreter.environment._
import org.scalameta._
import org.scalameta.invariants._

object Interpreter {

  def evalFunc(metaprogram: i.Tree, argss: Seq[Any]*)(implicit c: Context): Any =
    evalFunc(metaprogram, argss, Env(List(ListMap[i.Term.Name, Object]()), ListMap[i.Term.Name, Object]()))._1

  def eval(term: Term)(implicit c: Context): Any = {
    val (Object(v, tpe, fields), env) =
      eval(term, Env(List(ListMap[i.Term.Name, Object]()), ListMap[i.Term.Name, Object]()))
    v
  }

  private def evalFunc(metaprogram: i.Tree, argss: Seq[Seq[Any]], env: Env)(implicit c: Context): (Object, Env) = {
    val defn @ i.Defn.Def(mods, nme, _, paramss, _, body) = metaprogram
    val newEnv = (argss zip paramss).foldLeft(env) { (agg, p) =>
      (p._1 zip p._2).foldLeft(agg) { (agg1, pv) =>
        agg1.push(pv._2.name.asInstanceOf[i.Term.Name], Object(pv._1, pv._2.tpe))
      }
    }
    eval(body, newEnv)
  }

  def eval(term: Tree, env: Env)(implicit c: Context): (Object, Env) = {
    println(s"Evaluating: ${term.show[Code]}")
    term match {
      case i.Term.Block(stats) =>
        stats.foldLeft((Object((), t"Unit"), env))((res, stat) => eval(stat, res._2))

      case nme @ i.Term.Name(_) =>
        (env.lookup(nme), env)

      // TODO(non-reflective): dispatch to pattern matching in an elegant way
      case i.Defn.Val(mods, List(i.Pat.Var.Term(name)), tpe, body) =>
        val (res, newEnv) = eval(body, env)
        // TODO(non-reflective): depends of the context (method body or template)
        (Object((), t"Unit"), newEnv.push(name, res))

      case i.Lit.Byte(v) => (Object(v, t"Byte"), env)
      case i.Lit.Short(v) => (Object(v, t"Short"), env)
      case i.Lit.Int(v) => (Object(v, t"Int"), env)
      case i.Lit.Long(v) => (Object(v, t"Long"), env)
      case i.Lit.Float(v) => (Object(v, t"Float"), env)
      case i.Lit.Double(v) => (Object(v, t"Double"), env)
      case i.Lit.String(v) => (Object(v, t"String"), env)
      case i.Lit.Bool(v) => (Object(v, t"Boolean"), env)
      case i.Lit.Unit() => (Object((), t"Unit"), env)

      case i.Term.Tuple(terms) => // TODO all sizes
        val (res, env1) = evalSeq(terms, env)
        res match {
          case e1 :: e2 :: Nil => (Object((e1.ref, e2.ref), t"(${e1.tpe}, ${e2.tpe})"), env1)
          case e1 :: e2 :: e3 :: Nil => (Object((e1.ref, e2.ref, e3.ref), t"(${e1.tpe}, ${e2.tpe}, ${e3.tpe})"), env1)
          case _ => unreachable(debug(res))
        }

      case i.Term.Select((lhs: i.Term.Name), op) if lhs.isPackage =>
        (env.lookup(op), env)

      case trm @ i.Term.Select(lhs, op) if op.isObject =>
        (env.lookup(op), env)

      case i.Term.Select(lhs, op) =>
        val (lhsV, env1) = eval(lhs, env)
        val method = lhs.tpe.members.filter(_.name == op).head
        methodCall(method, lhsV, Seq(), env1)

      case i.Term.ApplyUnary(nme, lhs) =>
        val method = lhs.tpe.members.filter(_.name == nme).head
        val (lhsV, newEnv) = eval(lhs, env)
        methodCall(method, lhsV, Seq(), newEnv)

      case i.Term.ApplyInfix(lhs, op, _, args) =>
        val method = lhs.tpe.members.filter(_.name == op).head
        val (lhsV, newEnv) = eval(lhs, env)
        val (rhsVs, callEnv) = evalSeq(args, env)
        methodCall(method, lhsV, rhsVs, callEnv)

      case i.Term.Apply(fun @ i.Term.Select(lhs, rhs), args) if fun.isDef =>
        val (lhsV, newEnv) = eval(lhs, env)
        val method = lhs.tpe.members.filter(_.name == rhs.name).head
        val (rhsVs, callEnv) = evalSeq(args, env)
        methodCall(method, lhsV, rhsVs, callEnv)

      case i.Term.Apply(lhs, args) =>
        val method = lhs.tpe.defs("apply")
        val (lhsV, newEnv) = eval(lhs, env)
        val (rhsVs, callEnv) = evalSeq(args, env)
        methodCall(method, lhsV, rhsVs, callEnv)

      // TODO generalize
      case i.Term.ApplyType(i.Term.Select(lhs, i.Term.Name("asInstanceOf")), tpes) =>
        val (lhsV, env1) = eval(lhs, env)
        // TODO does not work
        // if (lhs.tpe.dealias <:< tpes.head.dealias) (lhsV, env1)
        if (true) (lhsV, env1)
        else throw new ClassCastException(s"!${lhs.tpe.dealias} <:< ${tpes.head}")

      case i.Term.If(cond, thn, elze) =>
        val (Object(cV: Boolean, _, _), cEnv: Env) = eval(cond, env)
        if (cV) eval(thn, cEnv)
        else eval(elze, cEnv)

      case func @ i.Defn.Def(mods, nme, _, paramss, retTp, body) =>
        // TODO multiple-functions
        val funcObj = Object(new Function1[Any, Any] {
          var resultEnv: Env = env // TODO fix
          def apply(x: Any) = {
            val (res, env1) = evalFunc(func, Seq(Seq(x)), resultEnv)
            resultEnv = env1
            res.ref
          }
        }, t"Any => Any")

        (funcObj, env.push(nme, funcObj))

      // TODO specialization
      case i.Term.Function(params, bodyTerm) =>
        // TODO all functions!
        // TODO store the returning env!
        // TODO proper type.

        (Object(new Function1[Any, Any] {
          var resultEnv: Env = _
          def apply(x: Any) = {
            val newEnv = params.foldLeft(env) { (aggEnv, param) =>
              aggEnv.push(param.name.asInstanceOf[i.Term.Name], Object(x, param.tpe))
            }
            val (res, resEnv) = eval(bodyTerm, newEnv)
            resultEnv = resEnv
            res.ref
          }
        }, t"Any => Any"), env)

      // String interpolators
      case i.Term.Interpolate(i.Term.Name("s"), strings, terms) =>
        val (evalTerms, env1) = evalSeq(terms, env)
        val (evalStrings, env2) = evalSeq(strings, env1)
        val res = evalStrings.map(_.ref).zipAll(evalTerms.map(_.ref.toString), "", "").foldLeft("")((res, s) => res + s._1 + s._2)
        (Object(res, t"String"), env2)

      // Pattern matching
      case i.Term.Match(lhs, cases) =>
        val (lhsV, env1) = eval(lhs, env)
        var env2 = env1
        cases.collectFirst({
          case i.Case(pattern, guard, block) if ({
            { val (res, env3) = evalPattern(lhsV, pattern, env2); env2 = env3; res.as[Boolean] } &&
              (guard.isEmpty || { val (res, env3) = evalGuard(lhsV, guard.get, env2); env2 = env3; res.as[Boolean] })
          }) =>
            eval(block, env2)
        }).getOrElse(throw new MatchError(lhsV.ref, lhsV.tpe)) // TODO this needs to be an evaluated exception

      case _ => unreachable(debug(term, term.show[Semantics]))
    }
  }

  def evalPattern(lhs: Object, pattern: i.Pat, env: Env)(implicit c: Context): (Object, Env) = pattern match {
    case i.Pat.Typed(x, tp) =>
      (Object(tp.tpe <:< lhs.tpe, t"Boolean"), extendEnv(x, lhs, env))
    case i.Pat.Wildcard() =>
      (Object(true, t"Boolean"), env)
    case _ => unreachable(debug(pattern, pattern.show[Semantics]))
  }

  def extendEnv(v: i.Tree, lhs: Object, env: Env)(implicit c: Context): Env = v match {
    case i.Pat.Var.Term(name) => env.push(name, lhs)
    case _ => unreachable(debug(v, v.show[Semantics]))
  }

  def evalGuard(lhs: Object, guard: i.Term, env: Env)(implicit x: Context): (Object, Env) = ???

  def evalSeq[T <: Tree](terms: Seq[T], env: Env)(implicit c: Context): (Seq[Object], Env) =
    terms.foldLeft((Seq[Object](), env)) { (acc, arg) =>
      val (res, newEnv) = eval(arg, acc._2)
      (acc._1 :+ res, newEnv)
    }

  def methodCall(member: Member, lhs: Object, rhs: Seq[Object], env: Env)(implicit c: Context): (Object, Env) = {
    member match {
      // abstract method
      case m @ i.Decl.Def(mods, _, _, paramss, _) => // interface
        methodCallByMods(mods, paramss, lhs, rhs, env)
      case i.Defn.Def(mods, _, _, paramss, _, i.Term.Name("???")) =>
        methodCallByMods(mods, paramss, lhs, rhs, env)
      case m @ i.Pat.Var.Term(nme) =>
        methodCallByMods(member.mods.asInstanceOf[scala.collection.Seq[scala.meta.internal.ast.Mod]], Nil, lhs, rhs, env)
      case _ => unreachable(debug(member, member.show[Semantics]))
    }
  }

  def methodCallByMods(mods: Seq[i.Mod], params: Seq[Seq[i.Term.Param]], lhs: Object, rhs: Seq[Object], env: Env)(implicit c: Context) = {
    val jvmMethod: String = mods.collect({ case i.Mod.Ffi(s) => s }).head
    val rgx = """(.*)\((.*), (.*), (.*)""".r
    jvmMethod.substring(0, jvmMethod.length - 1) match {
      case rgx("scalaIntrinsic", lhsJTp, nme, argsRetJTp) =>
        ScalaEmulator.emulate(List(lhsJTp, nme, argsRetJTp), lhs +: rhs, env)
      case rgx("jvmMethod", lhsJTp, nme, argsRetJTp) =>
        methodCallByJavaSignature(params, lhs, rhs, lhsJTp, nme, argsRetJTp, env)
      case _ => unreachable(debug(jvmMethod.substring(0, jvmMethod.length - 1)))
    }
  }

  def methodCallByJavaSignature(
    params: Seq[Seq[i.Term.Param]], lhs: Object, rhs: Seq[Object],
    lhsJTp: String, nme: String, argsRetJTp: String, env: Env)(implicit c: Context) = {
    println(params)
    val paramsRegex = """\((.*)\).*""".r
    val paramsRegex(jvmParams) = argsRetJTp
    val types = jvmParams.replaceAll("(L.*?;|I|B)", "$1,").split(",").toList.filterNot(_.trim.isEmpty).map(Utils.jvmTypeToClass)
    val jMethod = Utils.jvmTypeToClass(lhsJTp).getMethod(nme, types: _*)

    val repeated = params.exists(_.exists {
      case i.Term.Param(_, _, Some(i.Type.Arg.Repeated(_)), _) => true
      case _ => false
    })

    val (vLHS, vRHS) = (lhs.ref, rhs.map(_.ref))
    val args = if (repeated) // pack trailing arguments into a seq
      (vRHS.take(types.length - 1) :+ Seq(vRHS.drop(types.length - 1): _*)).toSeq
    else vRHS
    val jvmArgs = args.toSeq.asInstanceOf[Seq[AnyRef]]
    try {
      // FIX[Desugar]: This is for by name parameters
      val extendedRHS = (vLHS.getClass.toString, jMethod.getName()) match {
        case ("class scala.meta.internal.ast.Ctor$Ref$Name$", "apply") =>
          jvmArgs ++ Seq[AnyRef](scala.meta.internal.hygiene.Denotation.Zero, scala.meta.internal.hygiene.Sigma.Naive, scala.meta.Origin.None)
        case ("class scala.meta.internal.ast.Term$ApplyType$", "apply") =>
          jvmArgs ++ Seq[AnyRef](scala.meta.Origin.None)
        case ("class scala.meta.internal.ast.Name$Anonymous$", "apply") =>
          jvmArgs ++ Seq[AnyRef](scala.meta.internal.hygiene.Denotation.Zero, scala.meta.internal.hygiene.Sigma.Naive, scala.meta.Origin.None)
        case ("class scala.meta.internal.ast.Term$Param$", "apply") =>
          jvmArgs ++ Seq[AnyRef](scala.meta.Origin.None)
        case ("class scala.meta.internal.ast.Template$", "apply") =>
          jvmArgs ++ Seq[AnyRef](scala.meta.Origin.None)
        case ("class scala.meta.internal.ast.Term$New$", "apply") =>
          jvmArgs ++ Seq[AnyRef](scala.meta.Origin.None)
        case _ => jvmArgs
      }

      // FIX[Desugar]: Hardcode the implicit arguments!
      val hardWiredArgs = extendedRHS ++ ((lhsJTp, nme, argsRetJTp) match {
        case ("Lscala/collection/immutable/List;", "map", "(Lscala/Function1;Lscala/collection/generic/CanBuildFrom;)Ljava/lang/Object;") =>
          Seq(List.canBuildFrom[AnyRef])
        case ("Lscala/meta/semantic/Api$XtensionSemanticTypeRefDefn;", "defn", "(Lscala/meta/semantic/Context;)Lscala/meta/Member;") =>
          val context = env.stack.flatten.find(_._1.toString == "c").head._2.ref
          Seq(context.asInstanceOf[AnyRef])
        case ("Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "isClass", "(Lscala/meta/semantic/Context;)Z") =>
          val context = env.stack.flatten.find(_._1.toString == "c").head._2.ref
          Seq(context.asInstanceOf[AnyRef])
        case ("Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "isObject", "(Lscala/meta/semantic/Context;)Z") =>
          val context = env.stack.flatten.find(_._1.toString == "c").head._2.ref
          Seq(context.asInstanceOf[AnyRef])
        case ("Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "isTrait", "(Lscala/meta/semantic/Context;)Z") =>
          val context = env.stack.flatten.find(_._1.toString == "c").head._2.ref
          Seq(context.asInstanceOf[AnyRef])
        case ("Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "name", "(Lscala/meta/semantic/Context;)Lscala/meta/Name;") =>
          val context = env.stack.flatten.find(_._1.toString == "c").head._2.ref
          Seq(context.asInstanceOf[AnyRef])
        case ("Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "isSealed", "(Lscala/meta/semantic/Context;)Z") =>
          val context = env.stack.flatten.find(_._1.toString == "c").head._2.ref
          Seq(context.asInstanceOf[AnyRef])
        case ("Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "children", "(Lscala/meta/semantic/Context;)Lscala/collection/immutable/Seq;") =>
          val context = env.stack.flatten.find(_._1.toString == "c").head._2.ref
          Seq(context.asInstanceOf[AnyRef])
        case ("Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "isFinal", "(Lscala/meta/semantic/Context;)Z") =>
          val context = env.stack.flatten.find(_._1.toString == "c").head._2.ref
          Seq(context.asInstanceOf[AnyRef])
        case ("Lscala/meta/semantic/Api$XtensionSemanticMemberLike;", "isCase", "(Lscala/meta/semantic/Context;)Z") =>
          val context = env.stack.flatten.find(_._1.toString == "c").head._2.ref
          Seq(context.asInstanceOf[AnyRef])
        case ("Lscala/meta/semantic/Api$XtensionSemanticScopeLike;", "tparams", "(Lscala/meta/semantic/Context;)Lscala/collection/immutable/Seq;") =>
          val context = env.stack.flatten.find(_._1.toString == "c").head._2.ref
          Seq(context.asInstanceOf[AnyRef])
        case _ =>
          println("No implicit parameter for: " + (lhsJTp, nme, argsRetJTp))
          Seq()
      })

      // FIX[Desugar]: This part is for extension methods!
      val extendedLHS = (vLHS, jMethod.getName()) match {
        case (lhs: i.Type.Name, "defn") =>
          XtensionSemanticTypeRefDefn(lhs)
        case (lhs: i.Defn.Type, x) if Set("isClass", "isObject", "isTrait", "name") contains (x) =>
          XtensionSemanticMember(lhs)
        case (lhs: i.Defn.Trait, x) if Set("isClass", "isObject", "isTrait", "isSealed", "children") contains (x) =>
          XtensionSemanticMember(lhs)
        case (lhs: i.Defn.Class, x) if Set("isFinal", "isCase", "name") contains (x) =>
          XtensionSemanticMember(lhs)
        case (lhs: i.Defn.Class, x) if Set("tparams") contains (x) =>
          XtensionSemanticScope(lhs)
        case _ =>
          println("Not extended: " + (vLHS.getClass, jMethod.getName()))
          vLHS
      }

      println("Expected class is: " + Utils.jvmTypeToClass(lhsJTp))
      println("Class is:" + extendedLHS.getClass)
      (Object(jMethod.invoke(extendedLHS, hardWiredArgs: _*), t"List[Int]"), env)
    } catch {
      case e: java.lang.reflect.InvocationTargetException =>
        throw e.getCause()
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
