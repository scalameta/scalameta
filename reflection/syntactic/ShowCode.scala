package scala.reflect
package syntactic

import scala.reflect.core._, Aux._
import org.scalareflect.show.Show
import Show.{ sequence => s, repeat => r, indent => i, newline => n }
import scala.reflect.syntactic.SyntacticInfo._
import scala.{Seq => _}
import scala.collection.immutable.Seq

// TODO: occasional " ;" is annoying
// TODO: needs way more parens, esp in types and patterns
// TODO: soft wrapping
// TODO: one mega instance for tree isn't nice, maybe separate instances for leafs and inferred instances for branches
// TODO: strings and string interpolation needs to be smarter wrt " vs """

object ShowCode {
  def templ(templ: Template) =
    if (templ eq Template.empty) s()
    else if (templ.parents.nonEmpty || templ.early.nonEmpty) s(" extends ", templ)
    else s(" ", templ)

  def parens(t: Term) = t match {
    case _: Lit | _: Term.Ref | _: Term.Placeholder | _: Term.Tuple => s(t)
    case _ => s("(", t, ")")
  }

  // Branches
  implicit def showTree[T <: Tree]: Show[T] = Show {
    case t: Name => if (t.isBackquoted) s("`", t.value, "`") else s(t.value)

    // ParamType
    case t: ParamType.Repeated => s(t.tpe, "*")
    case t: ParamType.ByName   => s("=> ", t.tpe)

    // Type
    case t: Type.Project     => s(t.qual, "#", t.name)
    case t: Type.Select      => s(t.qual, ".", t.name)
    case t: Type.Singleton   => s(t.ref, ".type")
    case t: Type.SuperSelect =>
      s(t.qual.map { qual => s(qual, ".") }.getOrElse(s()),
        "super", t.supertpe.map { st => s("[", st, "]") }.getOrElse(s()),
        ".", t.selector)
    case t: Type.Annotate    => s(t.tpe, " ", t.mods)
    case t: Type.Apply       => s(t.tpe, "[", r(t.args, ", "), "]")
    case t: Type.ApplyInfix  => s(t.lhs, " ", t.op, " ", t.rhs)
    case t: Type.Compound    => s(r(t.tpes, " with "), " { ", r(t.refinement, "; "), " }")
    case t: Type.Existential => s(t.tpe, " forSome { ", r(t.quants, "; "), " }")
    case t: Type.Placeholder => s("_", t.bounds)
    case t: Type.Tuple       => s("(", r(t.elements, ", "), ")")
    case t: Type.Function    =>
      val pparams = if (t.params.size == 1) s(t.params.head) else s("(", r(t.params, ", "), ")")
      s(pparams, " => ", t.res)

    // Lit
    case _: Lit.True   => s("true")
    case _: Lit.False  => s("false")
    case t: Lit.Int    => s(t.value.toString)
    case t: Lit.Long   => s(t.value.toString)
    case t: Lit.Float  => s(t.value.toString)
    case t: Lit.Double => s(t.value.toString)
    case t: Lit.Char   => s(t.value.toString)
    case t: Lit.String => s("\"", t.value, "\"")
    case t: Lit.Symbol => s("'", t.value.name)
    case _: Lit.Null   => s("null")
    case _: Lit.Unit   => s("()")

    // Term
    case t: Term.This        => s(t.qual.map { qual => s(qual, ".") }.getOrElse(s()), "this")
    case t: Term.Select      => s(parens(t.qual), ".", t.selector)
    case t: Term.SuperSelect =>
      s(t.qual.map { qual => s(qual, ".") }.getOrElse(s()),
        "super", t.supertpe.map { st => s("[", st, "]") }.getOrElse(s()),
        ".", t.selector)
    case t: Term.Assign      => s(parens(t.lhs), " = ", t.rhs)
    case t: Term.Update      => s(parens(t.lhs), " = ", t.rhs)
    case t: Term.Return      => s("return ", t.expr.map(s(_)).getOrElse(s()))
    case t: Term.Throw       => s("throw ", t.expr)
    case t: Term.Ascribe     => s(t.expr, ": ", t.tpe)
    case t: Term.Annotate    => s(t.expr, ": ", t.mods)
    case t: Term.Tuple       => s("(", r(t.elements, ", "), ")")
    case t: Term.Block       =>
      import Term.{Block, Function}
      def pstats(s: Seq[Stmt.Block]) = r(s.map(i(_)), ";")
      t match {
        case Block(Function(Param.Named(name, tptopt, _, mods) :: Nil, Block(stats)) :: Nil) if mods.exists(_.isInstanceOf[Mod.Implicit]) =>
          s("{ implicit ", name, tptopt.map { tpt => s(": ", tpt) }.getOrElse(s()), " => ", pstats(stats), n("}"))
        case Block(Function(Param.Named(name, None, _, mods) :: Nil, Block(stats)) :: Nil) =>
          s("{ ", name, " => ", pstats(stats), n("}"))
        case Block(Function(Param.Anonymous(_, _) :: Nil, Block(stats)) :: Nil) =>
          s("{ _ => ", pstats(stats), n("}"))
        case Block(Function(params, Block(stats)) :: Nil) =>
          s("{ (", r(params, ", "), ") => ", pstats(stats), n("}"))
        case _ =>
          if (t.stats.isEmpty) s("{}") else s("{", pstats(t.stats), n("}"))
      }
    case t: Term.Cases       => s("{", r(t.cases.map(i(_)), ";"), n("}"))
    case t: Term.While       => s("while (", t.expr, ") ", t.body)
    case t: Term.Do          => s("do ", t.body, " while (", t.expr, ")")
    case t: Term.For         => s("for (", r(t.enums, "; "), ") ", t.body)
    case t: Term.ForYield    => s("for (", r(t.enums, "; "), ") yield ", t.body)
    case t: Term.New         => s("new", t.templ)
    case _: Term.Placeholder => s("_")
    case t: Term.Eta         => s(t.term, " _")
    case t: Term.Match       => s(t.scrut, " match ", t.cases)
    case t: Term.Apply       => s(parens(t.fun), t.args)
    case t: Term.ApplyType   => s(parens(t.fun), t.targs)
    case t: Term.ApplyUnary  => s(t.op, parens(t.arg))
    case t: Term.ApplyInfix  =>
      s(parens(t.lhs), " ", t.op, t.targs, " ", t.args match {
        case (arg: Term) :: Nil => s(parens(arg))
        case args               => s(args)
      })
    case t: Term.Try         =>
      s("try ", t.expr,
        t.catchp.map { catchp => s(" catch ", catchp) }.getOrElse(s()),
        t.finallyp.map { finallyp => s(" finally ", finallyp) }.getOrElse(s()))
    case t: Term.If.Then     => s("if (", t.cond, ") ", t.thenp)
    case t: Term.If.ThenElse => s("if (", t.cond, ") ", t.thenp, " else ", t.elsep)
    case t: Term.Function =>
      t match {
        case Term.Function(Param.Named(name, tptopt, _, mods) :: Nil, body) if mods.exists(_.isInstanceOf[Mod.Implicit]) =>
          s("implicit ", name, tptopt.map { tpt => s(": ", tpt) }.getOrElse(s()), " => ", body)
        case Term.Function(Param.Named(name, None, _, mods) :: Nil, body) =>
          s(name, " => ", body)
        case Term.Function(Param.Anonymous(_, _) :: Nil, body) =>
          s("_ => ", body)
        case Term.Function(params, body) =>
          s("(", r(params, ", "), ") => ", body)
      }
    case t: Term.Interpolate =>
      val zipped = t.parts.zip(t.args).map {
        case (part, id: Name) if !id.isBackquoted => s(part, "$", id.value)
        case (part, arg)                          => s(part, "${", arg, "}")
      }
      s(t.prefix, "\"", r(zipped), t.parts.last, "\"")

    // Pat
    case t: Pat.Alternative  => s(t.lhs, " | ", t.rhs)
    case t: Pat.Bind         => s(t.lhs, " @ ", t.rhs)
    case t: Pat.Tuple        => s(t.elements)
    case _: Pat.SeqWildcard  => s("_*")
    case t: Pat.Typed        => s(t.lhs, ": ", t.rhs)
    case _: Pat.Wildcard     => s("_")
    case t: Pat.Extract      => s(t.ref, t.targs, t.elements)
    case t: Pat.ExtractInfix =>
      s(t.lhs, " ", t.ref, " ", t.rhs match {
        case pat :: Nil => s(pat)
        case pats       => s(pats)
      })
    case t: Pat.Interpolate  =>
      val zipped = t.parts.zip(t.args).map {
        case (part, id: Name) if !id.isBackquoted => s(part, "$", id.value)
        case (part, arg)                          => s(part, "${", arg, "}")
      }
      s(t.prefix, "\"", r(zipped), t.parts.last, "\"")

    // Arg
    case t: Arg.Named    => s(t.name, " = ", t.rhs)
    case t: Arg.Repeated => s(t.arg, ": _*")

    // Mod
    case t: Mod.Annot         => s("@", t.tpe, t.argss, " ")
    case _: Mod.Abstract      => s("abstract ")
    case _: Mod.Case          => s("case ")
    case _: Mod.Covariant     => s("+")
    case _: Mod.Contravariant => s("-")
    case _: Mod.Doc           => ???
    case _: Mod.Final         => s("final ")
    case _: Mod.Implicit      => s("implicit ")
    case _: Mod.Lazy          => s("lazy ")
    case _: Mod.Macro         => s("macro ")
    case _: Mod.Override      => s("override ")
    case _: Mod.Sealed        => s("sealed ")
    case t: Mod.Private       => s("private", t.within, " ")
    case t: Mod.Protected     => s("protected", t.within, " ")
    case _: Mod.ValParam      => s("val ")
    case _: Mod.VarParam      => s("var ")

    // Defn
    case t: Defn.Val       => s(t.mods, "val ", r(t.pats, ", "), t.decltpe, " = ", t.rhs)
    case t: Defn.Var       => s(t.mods, "var ", r(t.pats, ", "), t.decltpe, " = ", t.rhs.map(s(_)).getOrElse(s("_")))
    case t: Defn.Type      => s(t.mods, "type ", t.name, t.tparams, " = ", t.body)
    case t: Defn.Class     => s(t.mods, "class ", t.name, t.tparams, t.ctor, templ(t.templ))
    case t: Defn.Trait     => s(t.mods, "trait ", t.name, t.tparams, templ(t.templ))
    case t: Defn.Object    => s(t.mods, "object ", t.name, templ(t.templ))
    case t: Defn.Def       =>
      s(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits), t.decltpe, " = ", t.body)
    case t: Defn.Procedure =>
      s(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits), " { ", r(t.stats.map(i(_)), ";"), n("}"))

    // Decl
    case t: Decl.Val       => s(t.mods, "val ", r(t.pats, ", "), t.decltpe)
    case t: Decl.Var       => s(t.mods, "var ", r(t.pats, ", "), t.decltpe)
    case t: Decl.Type      => s(t.mods, "type ", t.name, t.tparams, t.bounds)
    case t: Decl.Def       => s(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits), ": ", t.decltpe)
    case t: Decl.Procedure => s(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits))

    // Pkg
    case t: CompUnit   => s(r(t.refs.map(r => s("package ", r)), "\n"), "\n", r(t.stats, "\n"))
    case t: Pkg.Named  => s("package ", t.name, " { ", r(t.stats.map(i(_)), "\n"), n("}"))
    case t: Pkg.Object => s(t.mods, " package object ", t.name, templ(t.templ))

    // Ctor
    case t: Ctor.Primary   => s(t.mods, (t.explicits, t.implicits))
    case t: Ctor.Secondary =>
      s(t.mods, "def this", (t.explicits, t.implicits), t.stats match {
        case Nil   => s(" = this", t.primaryCtorArgss)
        case stats => s("{ this", t.primaryCtorArgss, ";", r(stats, "; "), " }")
      })

    // Enum
    case t: Enum.Val       => s(t.pat, " = ", t.rhs)
    case t: Enum.Generator => s(t.pat, " <- ", t.rhs)
    case t: Enum.Guard     => s("if ", t.cond)

    // Import
    case t: Import.Selector.Rename   => s(t.from, " => ", t.to)
    case t: Import.Selector.Name     => s(t.name)
    case t: Import.Selector.Unimport => s(t.name, " => _")
    case _: Import.Selector.Wildcard => s("_")
    case t: Import.Clause   => s(t.ref, ".", t.sels)
    case t: Import          => s("import ", r(t.clauses, ", "))

    // Aux
    case t: Parent => s(t.tpe, t.argss)
    case t: Self =>
      if (t.name.isEmpty && t.decltpe.isEmpty) s()
      else s(" ", t.name, t.decltpe, " => ")
    case t: Template =>
      if (t eq Template.empty) s()
      else {
        val pearly = if (t.early.isEmpty) s() else s("{ ", r(t.early, "; "), " } with ")
        val pbody = if ((t.self eq Self.empty) && t.stats.isEmpty) s()
                    else s("{", t.self, r(t.stats.map(i(_)), ";"), n("}"))
        val pparents = if (t.parents.nonEmpty) s(r(t.parents, " with "), " ") else s()
        s(pearly, pparents, pbody)
      }
    case t: TypeBounds =>
      s(t.lo.map { lo => s(" >: ", lo) }.getOrElse(s()),
        t.hi.map { hi => s(" <: ", hi) }.getOrElse(s()))
    case t: Case  =>
      s("case ", t.pat, t.cond.map { cond => s(" if ", cond) }.getOrElse(s()), " =>", r(t.stats.map(i(_)), ";"))
    case t: Param.Anonymous => s(t.mods, "_", t.decltpe)
    case t: Param.Named => s(t.mods, t.name, t.decltpe, t.default.map(s(" = ", _)).getOrElse(s()))
    case t: TypeParam.Anonymous =>
      val cbounds = r(t.contextBounds.map { s(": ", _) })
      val vbounds = r(t.contextBounds.map { s("<% ", _) })
      s(t.mods, "_", t.tparams, cbounds, vbounds, t.bounds)
    case t: TypeParam.Named =>
      val cbounds = r(t.contextBounds.map { s(": ", _) })
      val vbounds = r(t.contextBounds.map { s("<% ", _) })
      s(t.mods, t.name, t.tparams, cbounds, vbounds, t.bounds)
  }

  // Multiples and optionals
  implicit val showAccessQualifierOpt: Show[Option[Mod.AccessQualifier]] = Show { t =>
    t.map { qual => s("[", qual, "]") }.getOrElse(s())
  }
  implicit val showArgs: Show[Seq[Arg]] = Show {
    case (b: Term.Block) :: Nil => s(" ", b)
    case args                   => s("(", r(args, ", "), ")")
  }
  implicit val showArgss: Show[Seq[Seq[Arg]]] = Show { r(_) }
  implicit val showTargs: Show[Seq[Type]] = Show { targs =>
    if (targs.isEmpty) s()
    else s("[", r(targs, ", "), "]")
  }
  implicit val showPats: Show[Seq[Pat]] = Show { pats =>
    s("(", r(pats, ", "), ")")
  }
  implicit val showMods: Show[Seq[Mod]] = Show { mods =>
    if (mods.nonEmpty) r(mods) else s()
  }
  implicit def showParams[P <: Param]: Show[Seq[P]] = Show { params => s("(", r(params, ", "), ")") }
  implicit val showTparams: Show[Seq[TypeParam]] = Show { tparams =>
    if (tparams.nonEmpty) s("[", r(tparams, ", "), "]") else s()
  }
  implicit def showParamLists[P <: Param]: Show[(Seq[Seq[P]], Seq[P])] = Show { case (expl, impl) =>
    s(r(expl),
      if (impl.isEmpty) s()
      else s("(implicit ", r(impl, ", "), ")"))
  }
  implicit val showParamTypeOpt: Show[Option[ParamType]] = Show { _.map { t => s(": ", t) }.getOrElse(s()) }
  implicit val showTypeOpt: Show[Option[Type]] = Show { _.map { t => s(": ", t) }.getOrElse(s()) }
  implicit val showTermNameOpt: Show[Option[Term.Name]] = Show { _.map(s(_)).getOrElse(s(")")) }
  implicit val showImportSels: Show[Seq[Import.Selector]] = Show {
    case (t: Import.Selector.Name) :: Nil     => s(t)
    case (t: Import.Selector.Wildcard) :: Nil => s(t)
    case sels                                 => s("{ ", r(sels, ", "), " }")
  }
}
