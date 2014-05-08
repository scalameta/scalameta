package scala.reflect
package syntactic

import scala.reflect.core._, Aux._
import org.scalareflect.prettyprint.Print
import org.scalareflect.prettyprint.Print.{ seq => p, rep => r }
import scala.reflect.syntactic.SyntacticInfo._
import scala.{Seq => _}
import scala.collection.immutable.Seq

// TODO: what about Print[Member] and likes?
// TODO: also having independent instances makes it very difficult to communicate stuff like indentation
// TODO: make Print[T] invariant but generate instances based on generic cases

object Printers {
  // Branches
  implicit val printTypeRef: Print[Type.Ref] = Print {
    case t: Type.Name        => printName(t)
    case t: Type.Project     => p(t.qual, "#", t.name)
    case t: Type.Select      => p(t.qual, ".", t.name)
    case t: Type.Singleton   => p(t.ref, ".type")
    case t: Type.SuperSelect =>
      p(t.qual.map { qual => p(qual, ".") }.getOrElse(""),
        "super", t.supertpe.map { st => p("[", st, "]") }.getOrElse(""),
        ".", t.selector)
  }
  implicit val printParamType: Print[ParamType] = Print {
    case t: Type               => printType(t)
    case t: ParamType.Repeated => p(t.tpe, "*")
    case t: ParamType.ByName   => p("=> ", t.tpe)
  }
  implicit val printType: Print[Type] = Print {
    case t: Lit              => printLit(t)
    case t: Type.Ref         => printTypeRef(t)
    case t: Type.Annotate    => p(t.tpe, " ", t.mods)
    case t: Type.Apply       => p(t.tpe, "[", r(t.args, ", "), "]")
    case t: Type.Compound    => p(r(t.tpes, " with "), " { ", r(t.refinement, "; "), " }")
    case t: Type.Existential => p(t.tpe, " forSome { ", r(t.quants, "; "), " }")
    case t: Type.Function    => p("(", r(t.params, ", "), ") => ", t.res)
    case t: Type.Placeholder => p("_", t.bounds)
    case t: Type.Tuple       => p("(", r(t.elements, ", "), ")")
  }
  implicit val printLit: Print[Lit] = Print {
    case _: Lit.True   => "true"
    case _: Lit.False  => "true"
    case t: Lit.Int    => t.value.toString
    case t: Lit.Long   => t.value.toString
    case t: Lit.Float  => t.value.toString
    case t: Lit.Double => t.value.toString
    case t: Lit.Char   => t.value.toString
    case t: Lit.String => t.value
    case t: Lit.Symbol => p("'", t.value.name)
    case _: Lit.Null   => "null"
    case _: Lit.Unit   => "()"
  }
  implicit val printTermRefWithPat: Print[Term.Ref with Pat] = Print {
    case t: Term.Name        => printName(t)
    case t: Term.Select      => p(t.qual, ".", t.selector)
  }
  implicit val printTermRef: Print[Term.Ref] = Print {
    case t: Pat              => printTermRefWithPat(t)
    case t: Term.This        => p(t.qual.map { qual => p(qual, ".") }.getOrElse(""), "this")
    case t: Term.SuperSelect =>
      p(t.qual.map { qual => p(qual, ".") }.getOrElse(""),
        "super", t.supertpe.map { st => p("[", st, "]") }.getOrElse(""),
        ".", t.selector)
  }
  implicit val printTerm: Print[Term] = Print {
    case t: Lit              => printLit(t)
    case t: Term.Ref         => printTermRef(t)
    case t: Term.Apply       => p(t.fun, t.args)
    case t: Term.ApplyRight  => p(t.lhs, " ", t.op, t.targs, " ", t.rhs)
    case t: Term.ApplyType   => p(t.fun, t.targs)
    case t: Term.ApplyUnary  => p(t.op, t.arg)
    case t: Term.Assign      => p(t.lhs, " = ", t.rhs)
    case t: Term.Update      => p(t.lhs, " = ", t.rhs)
    case t: Term.Return      => p("return ", t.expr)
    case t: Term.Throw       => p("throw ", t.expr)
    case t: Term.Ascribe     => p(t.expr, ": ", t.tpe)
    case t: Term.Annotate    => p(t.expr, ": ", t.mods)
    case t: Term.Tuple       => p("(", r(t.elements, ", "), ")")
    case t: Term.Block       => if (t.stats.isEmpty) "{}" else p("{ ", r(t.stats, "; "), " }")
    case t: Term.Cases       => p("{ ", r(t.cases, "; "), " }")
    case t: Term.While       => p("while (", t.expr, ") ", t.body)
    case t: Term.Do          => p("do ", t.body, " while (", t.expr, ")")
    case t: Term.For         => p("for (", r(t.enums, "; "), ") ", t.body)
    case t: Term.ForYield    => p("for (", r(t.enums, "; "), ") yield ", t.body)
    case t: Term.New         => p("new", t.templ)
    case _: Term.Placeholder => "_"
    case t: Term.Eta         => p(t.term, " _")
    case t: Term.Match       => p(t.scrut, " match ", t.cases)
    case t: Term.Try         =>
      p("try ", t.expr,
        t.catchp.map { catchp => p(" catch ", catchp) }.getOrElse(""),
        t.finallyp.map { finallyp => p(" finally ", finallyp) }.getOrElse(""))
    case t: Term.If =>
      p("if (", t.cond, ") ", t.thenp, t.elsep.map { thenp => p(" then ", thenp) }.getOrElse(""))
    case t: Term.Function =>
      t match {
        case Term.Function(Param.Named(name, tptopt, _, mods) :: Nil, body) if mods.exists(_.isInstanceOf[Mod.Implicit]) =>
          p("{ implicit ", name, tptopt.map { tpt => p(": ", tpt) }.getOrElse(""), body)
        case Term.Function(Param.Anonymous(_, _) :: Nil, body) =>
          p("_ => ", body)
        case Term.Function(params, body) =>
          p("(", r(params, ","), ") => ", body)
      }
    case t: Term.Interpolate =>
      val zipped = t.parts.zip(t.args).map {
        case (part, id: Name) if !id.isBackquoted => p(part, "$", id.value)
        case (part, arg)                          => p(part, "${", arg, "}")
      }
      p(t.prefix, "\"", r(zipped, ""), t.parts.last, "\"")
  }
  implicit val printRef: Print[Ref] = Print {
    case t: Type.Ref => printTypeRef(t)
    case t: Term.Ref => printTermRef(t)
  }
  implicit val printPat: Print[Pat] = Print {
    case t: Lit             => printLit(t)
    case t: Term.Ref        => printTermRef(t)
    case t: Pat.Alternative => p(t.lhs, " | ", t.rhs)
    case t: Pat.Bind        => p(t.lhs, " @ ", t.rhs)
    case t: Pat.Extract     => p(t.ref, t.targs, t.elements)
    case t: Pat.Tuple       => p(t.elements)
    case _: Pat.SeqWildcard => "_*"
    case t: Pat.Typed       => p(t.lhs, ": ", t.rhs)
    case _: Pat.Wildcard    => "_"
    case t: Pat.Interpolate =>
      val zipped = t.parts.zip(t.args).map {
        case (part, id: Name) if !id.isBackquoted => p(part, "$", id.value)
        case (part, arg)                          => p(part, "${", arg, "}")
      }
      p(t.prefix, "\"", r(zipped, ""), t.parts.last, "\"")
  }
  implicit val printArg: Print[Arg] = Print {
    case t: Term         => printTerm(t)
    case t: Arg.Named    => p(t.name, " = ", t.rhs)
    case t: Arg.Repeated => p(t.arg, ": _*")
  }
  implicit val printMod: Print[Mod] = Print {
    case t: Mod.Annot         => p("@", t.tpe, t.argss)
    case _: Mod.Abstract      => "abstract"
    case _: Mod.Case          => "case"
    case _: Mod.Covariant     => "+"
    case _: Mod.Contravariant => "-"
    case _: Mod.Doc           => ???
    case _: Mod.Final         => "final"
    case _: Mod.Implicit      => "implicit"
    case _: Mod.Lazy          => "lazy"
    case _: Mod.Macro         => "macro"
    case _: Mod.Override      => "override"
    case _: Mod.Sealed        => "sealed"
    case t: Mod.Private       => p("private", t.within)
    case t: Mod.Protected     => p("protected", t.within)
    case _: Mod.ValParam      => "val"
    case _: Mod.VarParam      => "var"
  }
  implicit val printDefn: Print[Defn] = Print {
    case t: Defn.Val       => p(t.mods, "val ", r(t.pats, ", "), t.decltpe, " = ", t.rhs)
    case t: Defn.Var       => p(t.mods, "var ", r(t.pats, ", "), t.decltpe, " = ", t.rhs.map(p(_)).getOrElse("_"))
    case t: Defn.Type      => p(t.mods, "type ", t.name, t.tparams, " = ", t.body)
    case t: Defn.Def       => p(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits), ": ", t.decltpe, " = ", t.body)
    case t: Defn.Class     => p(t.mods, "class ", t.name, t.tparams, t.ctor, t.templ)
    case t: Defn.Trait     => p(t.mods, "trait ", t.name, t.tparams, t.templ)
    case t: Defn.Object    => p(t.mods, "object ", t.name, t.templ)
    case t: Defn.Procedure => p(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits), " { ", r(t.stats, "; "), " }")
  }
  implicit val printDecl: Print[Decl] = Print {
    case t: Decl.Val       => p(t.mods, "val ", r(t.pats, ", "), ": ", t.decltpe )
    case t: Decl.Var       => p(t.mods, "var ", r(t.pats, ", "), ": ", t.decltpe )
    case t: Decl.Type      => p(t.mods, "type ", t.name, t.tparams, t.bounds)
    case t: Decl.Def       => p(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits), ": ", t.decltpe)
    case t: Decl.Procedure => p(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits))
  }
  implicit val printPkg: Print[Pkg] = Print {
    case t: Pkg.Root   => p("package root { ", r(t.stats, "; "), " }")
    case t: Pkg.Empty  => r(t.stats, "; ")
    case t: Pkg.Named  => p("package ", t.name, " { ", r(t.stats, "; "), " }")
    case t: Pkg.Object => p(t.mods, " package object ", t.name, " extends ", t.templ)
  }
  implicit val printCtor: Print[Ctor] = Print {
    case t: Ctor.Primary   => p(t.mods, (t.explicits, t.implicits))
    case t: Ctor.Secondary =>
      p(t.mods, "def this", (t.explicits, t.implicits), t.stats match {
        case Nil   => p(" = this", t.primaryCtorArgss)
        case stats => p("{ this", t.primaryCtorArgss, ";", r(stats, "; "), " }")
      })
  }
  implicit def printEnum: Print[Enum] = Print {
    case t: Enum.Val       => p(t.pat, " = ", t.rhs)
    case t: Enum.Generator => p(t.pat, " <- ", t.rhs)
    case t: Enum.Guard     => p("if ", t.cond)
  }

  implicit val printTree: Print[Tree] = Print {
    case t: Term            => printTerm(t)
    case t: Pat             => printPat(t)
    case t: ParamType       => printParamType(t)
    case t: Arg             => printArg(t)
    case t: Mod             => printMod(t)
    case t: Defn            => printDefn(t)
    case t: Decl            => printDecl(t)
    case t: Pkg             => printPkg(t)
    case t: Ctor            => printCtor(t)
    case t: Enum            => printEnum(t)
    case t: Param           => printParam(t)
    case t: TypeParam       => printTypeParam(t)
    case t: Import.Selector => printImportSelector(t)
    case t: Import.Clause   => p(t.ref, ".", t.sels)
    case t: Import          => p("import ", r(t.clauses, ", "))
    case t: Parent          => p(t.tpe, t.argss)
    case t: Self            =>
      if (t.name.isEmpty && t.decltpe.isEmpty) ""
      else p(t.name, t.decltpe, " => ")
    case t: Template =>
      if (t eq Template.empty) ""
      else {
        val searly = if (t.early.isEmpty) "" else p("{ ", r(t.early, "; "), " } with ")
        val sbody = if ((t.self eq Self.empty) && t.stats.isEmpty) ""
                    else p("{ ", t.self, r(t.stats, "; "), " }")
        p(" ", searly, r(t.parents, " with "), sbody)
      }
    case t: TypeBounds =>
      p(t.lo.map { lo => p(" >: ", lo) }.getOrElse(""),
        t.hi.map { hi => p(" <: ", hi) }.getOrElse(""))
    case t: Case            =>
      p("case ", t.pat, t.cond.map { cond => p("if ", cond, " ") }.getOrElse(""), " =>", t.body.map(p(" ", _)).getOrElse(""))
  }
  implicit val printImportSelector: Print[Import.Selector] = Print {
    case t: Import.Selector.Rename   => p(t.from, " => ", t.to)
    case t: Import.Selector.Name     => t.name
    case t: Import.Selector.Unimport => p(t.name, " => _")
    case t: Import.Selector.Wildcard => "_"
  }
  implicit val printParam: Print[Param] = Print {
    case t: Param.Anonymous => p(t.mods, "_", t.decltpe)
    case t: Param.Named     => p(t.mods, t.name, t.decltpe, t.default.map(p(" = ", _)).getOrElse(""))
  }
  implicit val printTypeParam: Print[TypeParam] = Print {
    case t: TypeParam.Anonymous =>
      val cbounds = r(t.contextBounds.map { p(": ", _) }, "")
      val vbounds = r(t.contextBounds.map { p("<% ", _) }, "")
      p(t.mods, "_", t.tparams, cbounds, vbounds, t.bounds)
    case t: TypeParam.Named =>
      val cbounds = r(t.contextBounds.map { p(": ", _) }, "")
      val vbounds = r(t.contextBounds.map { p("<% ", _) }, "")
      p(t.mods, t.name, t.tparams, cbounds, vbounds, t.bounds)
  }
  implicit val printAccessQualifier: Print[Mod.AccessQualifier] = Print {
    case t: Term.This => p(t)
    case t: Name => p(t)
  }
  // something is broken around there
  implicit def printStmtBlock: Print[Stmt.Block] = Print {
    case t: Import => printTree(t)
    case t: Term   => printTerm(t)
    case t: Defn   => printDefn(t)
  }
  implicit def printStmtRefine: Print[Stmt.Refine] = Print {
    case t: Decl      => printDecl(t)
    case t: Defn.Type => printDefn(t)
  }
  implicit def printStmtExistential: Print[Stmt.Existential] = Print {
    case t: Decl.Val  => printDecl(t)
    case t: Decl.Type => printDecl(t)
  }
  implicit def printStmtTopLevel: Print[Stmt.TopLevel] = Print {
    case t: Pkg    => printPkg(t)
    case t: Defn   => printDefn(t)
    case t: Import => printTree(t)
  }
  implicit def printStmtTemplate: Print[Stmt.Template] = Print {
    case t: Defn           => printDefn(t)
    case t: Decl           => printDecl(t)
    case t: Ctor.Secondary => printCtor(t)
    case t: Term           => printTerm(t)
    case t: Import         => printTree(t)
  }

  // Base cases, multiples and optionals
  implicit val printString: Print[String] = Print { Predef.identity }
  implicit val printName: Print[Name] = Print { t => if (t.isBackquoted) p("`", t.value, "`") else t.value }
  implicit val printAccessQualifierOpt: Print[Option[Mod.AccessQualifier]] = Print { t =>
    t.map { qual => p("[", qual, "]") }.getOrElse("")
  }
  implicit val printArgs: Print[Seq[Arg]] = Print { args =>
    p("(", r(args, ", "), ")")
  }
  implicit val printArgss: Print[Seq[Seq[Arg]]] = Print { r(_, "") }
  implicit val printTargs: Print[Seq[Type]] = Print { targs =>
    if (targs.isEmpty) ""
    else p("[", r(targs, ", "), "]")
  }
  implicit val printPats: Print[Seq[Pat]] = Print { pats =>
    p("(", r(pats, ", "), ")")
  }
  implicit val printMods: Print[Seq[Mod]] = Print { mods =>
    if (mods.nonEmpty) p(r(mods, " "), " ") else ""
  }
  implicit val printParams: Print[Seq[Param]] = Print { params => p("(", r(params, ", "), ")") }
  implicit val printNamedParams: Print[Seq[Param.Named]] = Print { params => p("(", r(params, ", "), ")") }
  implicit val printAnonymousParams: Print[Seq[Param.Anonymous]] = Print { params => p("(", r(params, ", "), ")") }
  implicit val printTparams: Print[Seq[TypeParam]] = Print { tparams =>
    if (tparams.nonEmpty) p("[", r(tparams, ", "), "]") else ""
  }
  implicit val printParamLists: Print[(Seq[Seq[Param]], Seq[Param])] = Print { case (expl, impl) =>
    p(r(expl, ""),
      if (impl.isEmpty) ""
      else p("(implicit ", r(impl, ", "), ")"))
  }
  implicit val printParamTypeOpt: Print[Option[ParamType]] = Print { _.map { t => p(": ", t) }.getOrElse("") }
  implicit val printTypeOpt: Print[Option[Type]] = Print { _.map { t => p(": ", t) }.getOrElse("") }
  implicit val printTermNameOpt: Print[Option[Term.Name]] = Print { _.map(p(_)).getOrElse(")") }
  implicit val printImportSels: Print[Seq[Import.Selector]] = Print {
    case (t: Import.Selector.Name) :: Nil     => printImportSelector(t)
    case (t: Import.Selector.Wildcard) :: Nil => printImportSelector(t)
    case sels                                 => p("{ ", r(sels, ", "), " }")
  }
}
