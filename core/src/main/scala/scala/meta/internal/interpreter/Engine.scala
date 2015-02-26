package scala.meta.internal
package interpreter

import scala.meta._
import scala.meta.semantic.Context
import scala.meta.internal.{ ast => i }
import scala.meta.dialects.Scala211

import scala.collection.immutable.ListMap
import scala.meta.internal.interpreter.environment._

object Interpreter {
  def eval(term: Term)(implicit c: Context): Any = {
    val (Object(v, fields), env) =
      eval(term, Env(List(ListMap[Name, Object]()), ListMap[Name, Object]()))
    v
  }

  def eval(term: Tree, env: Env)(implicit c: Context): (Object, Env) = {
    term match {
      case i.Term.Block(stats) =>
        stats.foldLeft((Object(()), env))((res, stat) => eval(stat, res._2))

      case nme @ i.Term.Name(_) =>
        (env.lookup(nme), env)

      // TODO dispatch to pattern matching in an elegant way
      case i.Defn.Val(mods, List(i.Pat.Var.Term(name)), tpe, body) =>
        val (res, newEnv) = eval(body, env)
        // TODO depends of the context (method body or template)
        (Object(()), newEnv.push(name, res))

      // ASK Eugene if we can replace them with one!
      case i.Lit.Int(v) =>
        (Object(v), env)
      case i.Lit.String(v) =>
        (Object(v), env)

      // TODO order of execution method calls
      case i.Term.ApplyInfix(lhs, op, targs, args) =>
        val (lhsV, newEnv) = eval(lhs, env)
        // Method arguments in Scala are evaluated from left to right within a single parameter list.
        val (rhsVs, callEnv) = args.foldLeft(((Nil: List[Object]), newEnv)) { (acc, arg) =>
          val (res, newEnv) = eval(arg, acc._2)
          (res :: acc._1, newEnv)
        }

        // apply a method on the object by reflection
        val member = lhs.tpe.members.filter(_.name == op).head
        member match {
          case i.Defn.Def(_, _, tparams, args, retTp, body) =>
            body match {
              case i.Term.Apply(i.Term.Name("intrinsic"), lst) => // emulator
                ScalaEmulator.emulate(lst, lhsV :: rhsVs, callEnv)
              case i.Term.Apply(i.Term.Name("jvmMethod"), lst) => // reflective call
                ???
              case _ => // call method in the interpreter
                ???
            }
        }
      case _ => sys.error(s"""
        |unsupported tree:
        |${term.show[Code]}
        |${term.show[Raw]}
      """.trim.stripMargin)
    }
  }

}
