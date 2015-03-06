package scala.meta.internal
package interpreter

import scala.util.matching._
import scala.meta._
import scala.meta.semantic._
import scala.meta.internal.{ ast => i }
import scala.meta.dialects.Scala211

import scala.collection.immutable.ListMap
import scala.meta.internal.interpreter.environment._

object Interpreter {
  def eval(term: Term)(implicit c: Context): Any = {
    val (Object(v, tpe, fields), env) =
      eval(term, Env(List(ListMap[i.Term.Name, Object]()), ListMap[i.Term.Name, Object]()))
    v
  }

  def eval(term: Tree, env: Env)(implicit c: Context): (Object, Env) = {
    term match {
      case i.Term.Block(stats) =>
        stats.foldLeft((Object((), t"Unit"), env))((res, stat) => eval(stat, res._2))

      case nme @ i.Term.Name(_) =>
        (env.lookup(nme), env)

      // TODO dispatch to pattern matching in an elegant way
      case i.Defn.Val(mods, List(i.Pat.Var.Term(name)), tpe, body) =>
        val (res, newEnv) = eval(body, env)
        // TODO depends of the context (method body or template)
        (Object((), t"Unit"), newEnv.push(name, res))

      // ASK Eugene if we can replace them with one!
      case i.Lit.Int(v) =>
        (Object(v, t"Int"), env)
      case i.Lit.String(v) =>
        (Object(v, t"String"), env)
      case i.Lit.Bool(v) =>
        (Object(v, t"Boolean"), env)

      // TODO order of execution method calls
      case i.Term.ApplyInfix(lhs, op, _, args) =>
        val method = lhs.tpe.members.filter(_.name == op).head
        val (lhsV, newEnv) = eval(lhs, env)
        val (rhsVs, callEnv) = evalArgs(args, env)
        methodCall(method, lhsV, rhsVs, callEnv)

      case i.Term.Apply(lhs, args) =>
        val method = lhs.tpe.members.filter(_.name.toString == "apply").head

        val (lhsV, newEnv) = eval(lhs, env)
        val (rhsVs, callEnv) = evalArgs(args, env)
        methodCall(method, lhsV, rhsVs, callEnv)

      case i.Term.If(cond, thn, elze) =>
        val (Object(cV: Boolean, _, _), cEnv: Env) = eval(cond, env)
        if (cV) eval(thn, cEnv)
        else eval(elze, cEnv)

      // TODO specialization
      case i.Term.Function(params, bodyTerm) =>
        // TODO all functions!
        // TODO store the returning env!
        // TODO proper type.
        (Object(new Function1[Any, Any] {
          var resultEnv: Env = _
          def apply(x: Any) = {
            val newEnv = params.foldLeft(env)((aggEnv, param) => aggEnv.push(param.name.asInstanceOf[i.Term.Name], Object(x, param.tpe)))
            val (res, resEnv) = eval(bodyTerm, newEnv)
            resultEnv = resEnv
            res
          }
        }, t"Any => Any"), env)

      case _ => sys.error(s"""
        |unsupported tree:
        |${term.show[Code]}
        |${term.show[Raw]}
      """.trim.stripMargin)
    }
  }

  /*
   * Method arguments in Scala are evaluated from left to right within a single parameter list.
   */
  def evalArgs(args: Seq[i.Term.Arg], env: Env)(implicit c: Context): (List[Object], Env) = {
    args.foldLeft(((Nil: List[Object]), env)) { (acc, arg) =>
      val (res, newEnv) = eval(arg, acc._2)
      (acc._1 :+ res, newEnv)
    }
  }

  def methodCall(member: Member, lhs: Object, rhs: List[Object], env: Env)(implicit c: Context): (Object, Env) = {
    member match {
      // abstract method
      case m @ i.Decl.Def(mods, _, _, paramss, _) => // interface
        methodCallByMods(mods, paramss, lhs, rhs, env)
      case i.Defn.Def(mods, _, _, paramss, _, i.Term.Name("???")) =>
        methodCallByMods(mods, paramss, lhs, rhs, env)
    }
  }
  def methodCallByMods(mods: Seq[i.Mod], params: Seq[Seq[i.Term.Param]], lhs: Object, rhs: List[Object], env: Env)(implicit c: Context) = {
    val jvmMethod: String = mods.collect({ case i.Mod.Ffi(s) => s }).head
    val rgx = """(.*)\((.*), (.*), (.*)""".r
    jvmMethod.substring(0, jvmMethod.length - 1) match {
      case rgx("scalaIntrinsic", lhsJTp, nme, argsRetJTp) =>
        ScalaEmulator.emulate(List(lhsJTp, nme, argsRetJTp), lhs :: rhs, env)
      case rgx("jvmMethod", lhsJTp, nme, argsRetJTp) =>
        methodCallByJavaSignature(params, lhs, rhs, lhsJTp, nme, argsRetJTp, env)
      case _ => // call method in the interpreter
        ???
    }
  }
  def methodCallByJavaSignature(
    params: Seq[Seq[i.Term.Param]], lhs: Object, rhs: List[Object],
    lhsJTp: String, nme: String, argsRetJTp: String, env: Env)(implicit c: Context) = {
    val paramsRegex = """\((.*)\).*""".r
    val paramsRegex(jvmParams) = argsRetJTp
    val types = jvmParams.split(",").map(Utils.jvmTypeToClass).toList
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
      (Object(jMethod.invoke(vLHS, jvmArgs: _*), t"List[Int]"), env)
    } catch {
      case e: java.lang.reflect.InvocationTargetException =>
        throw e.getCause()
    }
  }
}

object Utils {
  def jvmTypeToClass(s: String): Class[_] =
    Class.forName(s.replaceAll("/", ".").subSequence(1, s.length - 1).toString)
}
