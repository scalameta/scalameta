package scala.meta
package syntactic.show

import scala.meta.Aux._
import org.scalameta.show.Show
import Show.{ sequence => s, repeat => r, indent => i, newline => n, meta => m }
import scala.meta.syntactic.parsers.SyntacticInfo._
import scala.meta.semantic._
import scala.{Seq => _}
import scala.collection.immutable.Seq

// TODO: occasional " ;" is annoying
// TODO: needs way more parens, esp in types and patterns
// TODO: soft wrapping
// TODO: one mega instance for tree isn't nice, maybe separate instances for leafs and inferred instances for branches
// TODO: strings and string interpolation needs to be smarter wrt " vs """

trait Code[T] extends Show[T]
object Code {
  def apply[T](f: T => Show.Result): Code[T] = new Code[T] { def apply(input: T) = f(input) }

  def templ(templ: Template) =
    // TODO: consider XXX.isEmpty
    if (templ.early.isEmpty && templ.parents.isEmpty && templ.self.name.isEmpty && templ.self.decltpe.isEmpty && templ.stats.isEmpty) s()
    else if (templ.parents.nonEmpty || templ.early.nonEmpty) s(" extends ", templ)
    else s(" ", templ)

  def p(oo: String, t: Qual.Term, left: Boolean = false, right: Boolean = false) = {
    def needsParens(oo: String, io: String): Boolean = {
      implicit class MySyntacticInfo(name: Name) {
        def myprecedence: Int = name.value match {
          case "." => 100
          case "()" => 100
          case "=" => 0
          case _ if name.myunary => 99
          case _ => name.precedence
        }
        def myunary: Boolean = name.value.startsWith("unary_")
      }
      val (op, ip) = (Term.Name(oo).myprecedence, Term.Name(io).myprecedence)
      val (oa, ia) = (Term.Name(oo).isLeftAssoc, Term.Name(io).isLeftAssoc)
      val (ou, iu) = (Term.Name(oo).myunary, Term.Name(io).myunary)
      val result = {
        if (ou && iu) true
        else if (oa ^ ia) true
        else if (oo == io && left != oa) true
        else op > ip
      }
      // println((oo, io, left, right) + " => " + (op, ip, oa, ia, ou, iu) + " => " + result)
      result
    }
    s(t) match {
      case Show.Meta(io: String, res) if needsParens(oo, io) => s("(", res, ")")
      case res => res
    }
  }

  // Branches
  // TODO: this match is not exhaustive: if I remove Mod.Package, then I get no warning
  implicit def codeTree[T <: Tree]: Code[T] = Code { x => (x: Tree) match {
    case t: Name => if (t.isBackquoted) s("`", t.value, "`") else s(t.value)

    // Param.Type
    case t: Param.Type.Repeated => s(t.tpe, "*")
    case t: Param.Type.ByName   => s("=> ", t.tpe)

    // Type
    case t: Type.Project     => s(t.qual, "#", t.selector)
    case t: Type.Select      => s(t.qual, ".", t.selector)
    case t: Type.Singleton   => s(t.ref, ".type")
    case t: Type.Annotate    => s(t.tpe, " ", t.mods)
    case t: Type.Apply       => s(t.tpe, "[", r(t.args, ", "), "]")
    case t: Type.ApplyInfix  => s(t.lhs, " ", t.op, " ", t.rhs)
    case t: Type.Compound    => s(r(t.tpes, "with"), if (t.hasRefinement) s(" { ", r(t.refinement, "; "), " }") else s())
    case t: Type.Existential => s(t.tpe, " forSome { ", r(t.quants, "; "), " }")
    case t: Type.Placeholder => s("_", t.bounds)
    case t: Type.Tuple       => s("(", r(t.elements, ", "), ")")
    case t: Type.Function    =>
      val pparams = if (t.params.size == 1) s(t.params.head) else s("(", r(t.params, ", "), ")")
      s(pparams, " => ", t.res)

    // Lit
    case t: Lit.Bool     => s(t.value.toString)
    case t: Lit.Int      => s(t.value.toString)
    case t: Lit.Long     => s(t.value.toString)
    case t: Lit.Float    => s(t.value.toString)
    case t: Lit.Double   => s(t.value.toString)
    case t: Lit.Char     => s(t.value.toString)
    case t: Lit.String   => s("\"", t.value, "\"")
    case t: Lit.Symbol   => s("'", t.value.name)
    case _: Lit.Null     => s("null")
    case _: Lit.Unit     => s("()")

    // Term
    case t: Term.This     => s(t.qual.map { qual => s(qual, ".") }.getOrElse(s()), "this")
    case t: Term.Select   => s(p(".", t.qual), if (t.isPostfix) " " else ".", t.selector)
    case t: Term.Assign   => m("=", s(p("=", t.lhs), " = ", t.rhs))
    case t: Term.Update   => m("=", s(p("=", t.lhs), " = ", t.rhs))
    case t: Term.Return   => s("return", if (t.hasExpr) s(" ", t.expr) else s())
    case t: Term.Throw    => s("throw ", t.expr)
    case t: Term.Ascribe  => s(t.expr, ": ", t.tpe)
    case t: Term.Annotate => s(t.expr, ": ", t.mods)
    case t: Term.Tuple    => s("(", r(t.elements, ", "), ")")
    case t: Term.Block    =>
      import Term.{Block, Function}
      def pstats(s: Seq[Stmt.Block]) = r(s.map(i(_)), ";")
      t match {
        case Block(Function(Param.Named(mods, name, tptopt, _) :: Nil, Block(stats)) :: Nil) if mods.exists(_.isInstanceOf[Mod.Implicit]) =>
          s("{ implicit ", name, tptopt.map { tpt => s(": ", tpt) }.getOrElse(s()), " => ", pstats(stats), n("}"))
        case Block(Function(Param.Named(mods, name, None, _) :: Nil, Block(stats)) :: Nil) =>
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
    case t: Term.New         => s("new ", t.templ)
    case _: Term.Placeholder => s("_")
    case t: Term.Eta         => s(t.term, " _")
    case t: Term.Match       => s(t.scrut, " match ", t.cases)
    case t: Term.Apply       => m("()", s(p("()", t.fun), t.args))
    case t: Term.ApplyType   => s(t.fun, t.targs)
    case t: Term.ApplyUnary  => m("unary_" + t.op.value, s(t.op, p("unary_" + t.op.value, t.arg)))
    case t: Term.ApplyInfix  =>
      m(t.op.value, s(p(t.op.value, t.lhs, left = true), " ", t.op, t.targs, " ", t.args match {
        case (arg: Term) :: Nil => s(p(t.op.value, arg, right = true))
        case args               => s(args)
      }))
    case t: Term.Try      =>
      s("try ", t.expr,
        t.catchp.map { catchp => s(" catch ", catchp) }.getOrElse(s()),
        t.finallyp.map { finallyp => s(" finally ", finallyp) }.getOrElse(s()))
    case t: Term.If       => s("if (", t.cond, ") ", t.thenp, if (t.hasElsep) s(" else ", t.elsep) else s())
    case t: Term.Function =>
      t match {
        case Term.Function(Param.Named(mods, name, tptopt, _) :: Nil, body) if mods.exists(_.isInstanceOf[Mod.Implicit]) =>
          s("implicit ", name, tptopt.map { tpt => s(": ", tpt) }.getOrElse(s()), " => ", body)
        case Term.Function(Param.Named(mods, name, None, _) :: Nil, body) =>
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
    case _: Mod.Override      => s("override ")
    case _: Mod.Sealed        => s("sealed ")
    case t: Mod.Private       => s("private", t.within, " ")
    case t: Mod.Protected     => s("protected", t.within, " ")
    case _: Mod.ValParam      => s("val ")
    case _: Mod.VarParam      => s("var ")
    case _: Mod.Package       => s("package ")

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
    case t: Defn.Macro     =>
      s(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits), ": ", t.tpe, " = macro ", t.body)

    // Decl
    case t: Decl.Val       => s(t.mods, "val ", r(t.pats, ", "), ": ", t.decltpe)
    case t: Decl.Var       => s(t.mods, "var ", r(t.pats, ", "), ": ", t.decltpe)
    case t: Decl.Type      => s(t.mods, "type ", t.name, t.tparams, t.bounds)
    case t: Decl.Def       => s(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits), ": ", t.decltpe)
    case t: Decl.Procedure => s(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits))

    // Pkg
    case t: CompUnit           => r(t.stats, "\n")
    case t: Pkg if t.hasBraces => s("package ", t.name, " { ", r(t.stats.map(i(_)), "\n"), n("}"))
    case t: Pkg                => s("package ", t.name, r(t.stats.map(n(_))))

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
    case t: Import.Rename   => s(t.from, " => ", t.to)
    case t: Import.Unimport => s(t.name, " => _")
    case _: Import.Wildcard => s("_")
    case t: Import.Clause   => s(t.ref, ".", t.sels)
    case t: Import          => s("import ", r(t.clauses, ", "))

    // Aux
    case t: Parent => s(t.tpe, t.argss)
    case t: Self =>
      if (t.name.isEmpty && t.decltpe.isEmpty) s()
      else s(" ", t.name, t.decltpe, " => ")
    case t: Template =>
      if (t.early.isEmpty && t.parents.isEmpty && t.self.name.isEmpty && t.self.decltpe.isEmpty && t.stats.isEmpty) s()
      else {
        val pearly = if (t.early.isEmpty) s() else s("{ ", r(t.early, "; "), " } with ")
        // TODO: use Template.hasBraces
        val pbody = if (t.self.name.isEmpty && t.self.decltpe.isEmpty && t.stats.isEmpty) s()
                    else s("{", t.self, r(t.stats.map(i(_)), ";"), n("}"))
        val pparents = if (t.parents.nonEmpty) s(r(t.parents, " with "), " ") else s()
        s(pearly, pparents, pbody)
      }
    case t: TypeBounds =>
      s(if (t.hasLo) s(" >: ", t.lo) else s(), if (t.hasHi) s(" <: ", t.hi) else s())
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
    case t: Qual.Super =>
      s(t.thisp.map { thisp => s(thisp, ".") }.getOrElse(s()),
        "super", t.superp.map { st => s("[", st, "]") }.getOrElse(s()))
  } }

  // Multiples and optionals
  implicit val codeAccessQualifierOpt: Code[Option[Mod.AccessQualifier]] = Code { t =>
    t.map { qual => s("[", qual, "]") }.getOrElse(s())
  }
  implicit val codeArgs: Code[Seq[Arg]] = Code {
    case (b: Term.Block) :: Nil => s(" ", b)
    case args                   => s("(", r(args, ", "), ")")
  }
  implicit val codeArgss: Code[Seq[Seq[Arg]]] = Code { r(_) }
  implicit val codeTargs: Code[Seq[Type]] = Code { targs =>
    if (targs.isEmpty) s()
    else s("[", r(targs, ", "), "]")
  }
  implicit val codePats: Code[Seq[Pat]] = Code { pats =>
    s("(", r(pats, ", "), ")")
  }
  implicit val codeMods: Code[Seq[Mod]] = Code { mods =>
    if (mods.nonEmpty) r(mods) else s()
  }
  implicit def codeParams[P <: Param]: Code[Seq[P]] = Code { params => s("(", r(params, ", "), ")") }
  implicit val codeTparams: Code[Seq[TypeParam]] = Code { tparams =>
    if (tparams.nonEmpty) s("[", r(tparams, ", "), "]") else s()
  }
  implicit def codeParamLists[P <: Param]: Code[(Seq[Seq[P]], Seq[P])] = Code { case (expl, impl) =>
    if (expl.isEmpty && impl.isEmpty) s()
    else s(r(expl),
      if (impl.isEmpty) s()
      else s("(implicit ", r(impl, ", "), ")"))
  }
  implicit val codeParamTypeOpt: Code[Option[Param.Type]] = Code { _.map { t => s(": ", t) }.getOrElse(s()) }
  implicit val codeTypeOpt: Code[Option[Type]] = Code { _.map { t => s(": ", t) }.getOrElse(s()) }
  implicit val codeTermNameOpt: Code[Option[Term.Name]] = Code { _.map(s(_)).getOrElse(s(")")) }
  implicit val codeImportSels: Code[Seq[Import.Selector]] = Code {
    case (t: Import.Name) :: Nil     => s(t)
    case (t: Import.Wildcard) :: Nil => s(t)
    case sels                        => s("{ ", r(sels, ", "), " }")
  }
}
