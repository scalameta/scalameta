package scala.reflect
package syntactic

import scala.reflect.core._, Aux._
import org.scalareflect.prettyprint.Print
import org.scalareflect.prettyprint.Print.{ sequence => p, repeat => r, indent => i, newline => n }
import scala.reflect.syntactic.SyntacticInfo._
import scala.{Seq => _}
import scala.collection.immutable.Seq

// TODO: " ;"


// TODO: what about Print[Member] and likes?
// TODO: also having independent instances makes it very difficult to communicate stuff like indentation
// TODO: make Print[T] invariant but generate instances based on generic cases

object Printers {
  def templ(templ: Template) =
    if (templ eq Template.empty) p()
    else if (templ.parents.nonEmpty || templ.early.nonEmpty) p(" extends ", templ)
    else p(" ", templ)

  def parens(t: Term) = t match {
    case _: Lit | _: Term.Ref | _: Term.Placeholder | _: Term.Tuple => p(t)
    case _ => p("(", t, ")")
  }
  def parens(t: Type) = p("(", t, ")")

  // Branches
  implicit def printTree[T <: Tree]: Print[T] = Print {
    case t: Name => if (t.isBackquoted) p("`", t.value, "`") else p(t.value)

    // ParamType
    case t: ParamType.Repeated => p(t.tpe, "*")
    case t: ParamType.ByName   => p("=> ", t.tpe)

    // Type
    case t: Type.Project     => p(t.qual, "#", t.name)
    case t: Type.Select      => p(t.qual, ".", t.name)
    case t: Type.Singleton   => p(t.ref, ".type")
    case t: Type.SuperSelect =>
      p(t.qual.map { qual => p(qual, ".") }.getOrElse(p()),
        "super", t.supertpe.map { st => p("[", st, "]") }.getOrElse(p()),
        ".", t.selector)
    case t: Type.Annotate    => p(t.tpe, " ", t.mods)
    case t: Type.Apply       => p(t.tpe, "[", r(t.args, ", "), "]")
    case t: Type.ApplyInfix  => p(t.lhs, " ", t.op, " ", t.rhs)
    case t: Type.Compound    => p(r(t.tpes, " with "), " { ", r(t.refinement, "; "), " }")
    case t: Type.Existential => p(t.tpe, " forSome { ", r(t.quants, "; "), " }")
    case t: Type.Placeholder => p("_", t.bounds)
    case t: Type.Tuple       => p("(", r(t.elements, ", "), ")")
    case t: Type.Function    =>
      val pparams = if (t.params.size == 1) p(t.params.head) else p("(", r(t.params, ", "), ")")
      p(pparams, " => ", t.res)

    // Lit
    case _: Lit.True   => p("true")
    case _: Lit.False  => p("false")
    case t: Lit.Int    => p(t.value.toString)
    case t: Lit.Long   => p(t.value.toString)
    case t: Lit.Float  => p(t.value.toString)
    case t: Lit.Double => p(t.value.toString)
    case t: Lit.Char   => p(t.value.toString)
    case t: Lit.String => p("\"", t.value, "\"")
    case t: Lit.Symbol => p("'", t.value.name)
    case _: Lit.Null   => p("null")
    case _: Lit.Unit   => p("()")

    // Term
    case t: Term.This        => p(t.qual.map { qual => p(qual, ".") }.getOrElse(p()), "this")
    case t: Term.Select      => p(parens(t.qual), ".", t.selector)
    case t: Term.SuperSelect =>
      p(t.qual.map { qual => p(qual, ".") }.getOrElse(p()),
        "super", t.supertpe.map { st => p("[", st, "]") }.getOrElse(p()),
        ".", t.selector)
    case t: Term.Assign      => p(parens(t.lhs), " = ", t.rhs)
    case t: Term.Update      => p(parens(t.lhs), " = ", t.rhs)
    case t: Term.Return      => p("return ", t.expr.map(p(_)).getOrElse(p()))
    case t: Term.Throw       => p("throw ", t.expr)
    case t: Term.Ascribe     => p(t.expr, ": ", t.tpe)
    case t: Term.Annotate    => p(t.expr, ": ", t.mods)
    case t: Term.Tuple       => p("(", r(t.elements, ", "), ")")
    case t: Term.Block       =>
      import Term.{Block, Function}
      def pstats(s: Seq[Stmt.Block]) = r(s.map(i(_)), ";")
      t match {
        case Block(Function(Param.Named(name, tptopt, _, mods) :: Nil, Block(stats)) :: Nil) if mods.exists(_.isInstanceOf[Mod.Implicit]) =>
          p("{ implicit ", name, tptopt.map { tpt => p(": ", tpt) }.getOrElse(p()), " => ", pstats(stats), n("}"))
        case Block(Function(Param.Named(name, None, _, mods) :: Nil, Block(stats)) :: Nil) =>
          p("{ ", name, " => ", pstats(stats), n("}"))
        case Block(Function(Param.Anonymous(_, _) :: Nil, Block(stats)) :: Nil) =>
          p("{ _ => ", pstats(stats), n("}"))
        case Block(Function(params, Block(stats)) :: Nil) =>
          p("{ (", r(params, ", "), ") => ", pstats(stats), n("}"))
        case _ =>
          if (t.stats.isEmpty) p("{}") else p("{", pstats(t.stats), n("}"))
      }
    case t: Term.Cases       => p("{", r(t.cases.map(i(_)), ";"), n("}"))
    case t: Term.While       => p("while (", t.expr, ") ", t.body)
    case t: Term.Do          => p("do ", t.body, " while (", t.expr, ")")
    case t: Term.For         => p("for (", r(t.enums, "; "), ") ", t.body)
    case t: Term.ForYield    => p("for (", r(t.enums, "; "), ") yield ", t.body)
    case t: Term.New         => p("new", t.templ)
    case _: Term.Placeholder => p("_")
    case t: Term.Eta         => p(t.term, " _")
    case t: Term.Match       => p(t.scrut, " match ", t.cases)
    case t: Term.Apply       => p(parens(t.fun), t.args)
    case t: Term.ApplyType   => p(parens(t.fun), t.targs)
    case t: Term.ApplyUnary  => p(t.op, parens(t.arg))
    case t: Term.ApplyInfix  =>
      p(parens(t.lhs), " ", t.op, t.targs, " ", t.args match {
        case (arg: Term) :: Nil => p(parens(arg))
        case args               => p(args)
      })
    case t: Term.Try         =>
      p("try ", t.expr,
        t.catchp.map { catchp => p(" catch ", catchp) }.getOrElse(p()),
        t.finallyp.map { finallyp => p(" finally ", finallyp) }.getOrElse(p()))
    case t: Term.If =>
      p("if (", t.cond, ") ", t.thenp,
        t.elsep.map { p(" else ", _) }.getOrElse(p()))
    case t: Term.Function =>
      t match {
        case Term.Function(Param.Named(name, tptopt, _, mods) :: Nil, body) if mods.exists(_.isInstanceOf[Mod.Implicit]) =>
          p("implicit ", name, tptopt.map { tpt => p(": ", tpt) }.getOrElse(p()), " => ", body)
        case Term.Function(Param.Named(name, None, _, mods) :: Nil, body) =>
          p(name, " => ", body)
        case Term.Function(Param.Anonymous(_, _) :: Nil, body) =>
          p("_ => ", body)
        case Term.Function(params, body) =>
          p("(", r(params, ", "), ") => ", body)
      }
    case t: Term.Interpolate =>
      val zipped = t.parts.zip(t.args).map {
        case (part, id: Name) if !id.isBackquoted => p(part, "$", id.value)
        case (part, arg)                          => p(part, "${", arg, "}")
      }
      p(t.prefix, "\"", r(zipped), t.parts.last, "\"")

    // Pat
    case t: Pat.Alternative  => p(t.lhs, " | ", t.rhs)
    case t: Pat.Bind         => p(t.lhs, " @ ", t.rhs)
    case t: Pat.Tuple        => p(t.elements)
    case _: Pat.SeqWildcard  => p("_*")
    case t: Pat.Typed        => p(t.lhs, ": ", t.rhs)
    case _: Pat.Wildcard     => p("_")
    case t: Pat.Extract      => p(t.ref, t.targs, t.elements)
    case t: Pat.ExtractInfix =>
      p(t.lhs, " ", t.ref, " ", t.rhs match {
        case pat :: Nil => p(pat)
        case pats       => p(pats)
      })
    case t: Pat.Interpolate  =>
      val zipped = t.parts.zip(t.args).map {
        case (part, id: Name) if !id.isBackquoted => p(part, "$", id.value)
        case (part, arg)                          => p(part, "${", arg, "}")
      }
      p(t.prefix, "\"", r(zipped), t.parts.last, "\"")

    // Arg
    case t: Arg.Named    => p(t.name, " = ", t.rhs)
    case t: Arg.Repeated => p(t.arg, ": _*")

    // Mod
    case t: Mod.Annot         => p("@", t.tpe, t.argss, " ")
    case _: Mod.Abstract      => p("abstract ")
    case _: Mod.Case          => p("case ")
    case _: Mod.Covariant     => p("+")
    case _: Mod.Contravariant => p("-")
    case _: Mod.Doc           => ???
    case _: Mod.Final         => p("final ")
    case _: Mod.Implicit      => p("implicit ")
    case _: Mod.Lazy          => p("lazy ")
    case _: Mod.Macro         => p("macro ")
    case _: Mod.Override      => p("override ")
    case _: Mod.Sealed        => p("sealed ")
    case t: Mod.Private       => p("private", t.within, " ")
    case t: Mod.Protected     => p("protected", t.within, " ")
    case _: Mod.ValParam      => p("val ")
    case _: Mod.VarParam      => p("var ")

    // Defn
    case t: Defn.Val       => p(t.mods, "val ", r(t.pats, ", "), t.decltpe, " = ", t.rhs)
    case t: Defn.Var       => p(t.mods, "var ", r(t.pats, ", "), t.decltpe, " = ", t.rhs.map(p(_)).getOrElse(p("_")))
    case t: Defn.Type      => p(t.mods, "type ", t.name, t.tparams, " = ", t.body)
    case t: Defn.Class     => p(t.mods, "class ", t.name, t.tparams, t.ctor, templ(t.templ))
    case t: Defn.Trait     => p(t.mods, "trait ", t.name, t.tparams, templ(t.templ))
    case t: Defn.Object    => p(t.mods, "object ", t.name, templ(t.templ))
    case t: Defn.Def       =>
      p(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits), t.decltpe, " = ", t.body)
    case t: Defn.Procedure =>
      p(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits), " { ", r(t.stats.map(i(_)), ";"), n("}"))

    // Decl
    case t: Decl.Val       => p(t.mods, "val ", r(t.pats, ", "), t.decltpe)
    case t: Decl.Var       => p(t.mods, "var ", r(t.pats, ", "), t.decltpe)
    case t: Decl.Type      => p(t.mods, "type ", t.name, t.tparams, t.bounds)
    case t: Decl.Def       => p(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits), ": ", t.decltpe)
    case t: Decl.Procedure => p(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits))

    // Pkg
    case t: CompUnit   => p(r(t.refs.map(r => p("package ", r)), "\n"), "\n", r(t.stats, "\n"))
    case t: Pkg.Named  => p("package ", t.name, " { ", r(t.stats.map(i(_)), "\n"), n("}"))
    case t: Pkg.Object => p(t.mods, " package object ", t.name, templ(t.templ))

    // Ctor
    case t: Ctor.Primary   => p(t.mods, (t.explicits, t.implicits))
    case t: Ctor.Secondary =>
      p(t.mods, "def this", (t.explicits, t.implicits), t.stats match {
        case Nil   => p(" = this", t.primaryCtorArgss)
        case stats => p("{ this", t.primaryCtorArgss, ";", r(stats, "; "), " }")
      })

    // Enum
    case t: Enum.Val       => p(t.pat, " = ", t.rhs)
    case t: Enum.Generator => p(t.pat, " <- ", t.rhs)
    case t: Enum.Guard     => p("if ", t.cond)

    // Import
    case t: Import.Selector.Rename   => p(t.from, " => ", t.to)
    case t: Import.Selector.Name     => p(t.name)
    case t: Import.Selector.Unimport => p(t.name, " => _")
    case _: Import.Selector.Wildcard => p("_")
    case t: Import.Clause   => p(t.ref, ".", t.sels)
    case t: Import          => p("import ", r(t.clauses, ", "))

    // Aux
    case t: Parent => p(t.tpe, t.argss)
    case t: Self =>
      if (t.name.isEmpty && t.decltpe.isEmpty) p()
      else p(" ", t.name, t.decltpe, " => ")
    case t: Template =>
      if (t eq Template.empty) p()
      else {
        val pearly = if (t.early.isEmpty) p() else p("{ ", r(t.early, "; "), " } with ")
        val pbody = if ((t.self eq Self.empty) && t.stats.isEmpty) p()
                    else p("{", t.self, r(t.stats.map(i(_)), ";"), n("}"))
        val pparents = if (t.parents.nonEmpty) p(r(t.parents, " with "), " ") else p()
        p(pearly, pparents, pbody)
      }
    case t: TypeBounds =>
      p(t.lo.map { lo => p(" >: ", lo) }.getOrElse(p()),
        t.hi.map { hi => p(" <: ", hi) }.getOrElse(p()))
    case t: Case  =>
      p("case ", t.pat, t.cond.map { cond => p(" if ", cond) }.getOrElse(p()), " =>", r(t.stats.map(i(_)), ";"))
    case t: Param.Anonymous => p(t.mods, "_", t.decltpe)
    case t: Param.Named => p(t.mods, t.name, t.decltpe, t.default.map(p(" = ", _)).getOrElse(p()))
    case t: TypeParam.Anonymous =>
      val cbounds = r(t.contextBounds.map { p(": ", _) })
      val vbounds = r(t.contextBounds.map { p("<% ", _) })
      p(t.mods, "_", t.tparams, cbounds, vbounds, t.bounds)
    case t: TypeParam.Named =>
      val cbounds = r(t.contextBounds.map { p(": ", _) })
      val vbounds = r(t.contextBounds.map { p("<% ", _) })
      p(t.mods, t.name, t.tparams, cbounds, vbounds, t.bounds)
  }

  // Multiples and optionals
  implicit val printAccessQualifierOpt: Print[Option[Mod.AccessQualifier]] = Print { t =>
    t.map { qual => p("[", qual, "]") }.getOrElse(p())
  }
  implicit val printArgs: Print[Seq[Arg]] = Print {
    case (b: Term.Block) :: Nil => p(" ", b)
    case args                   => p("(", r(args, ", "), ")")
  }
  implicit val printArgss: Print[Seq[Seq[Arg]]] = Print { r(_) }
  implicit val printTargs: Print[Seq[Type]] = Print { targs =>
    if (targs.isEmpty) p()
    else p("[", r(targs, ", "), "]")
  }
  implicit val printPats: Print[Seq[Pat]] = Print { pats =>
    p("(", r(pats, ", "), ")")
  }
  implicit val printMods: Print[Seq[Mod]] = Print { mods =>
    if (mods.nonEmpty) r(mods) else p()
  }
  implicit def printParams[P <: Param]: Print[Seq[P]] = Print { params => p("(", r(params, ", "), ")") }
  implicit val printTparams: Print[Seq[TypeParam]] = Print { tparams =>
    if (tparams.nonEmpty) p("[", r(tparams, ", "), "]") else p()
  }
  implicit def printParamLists[P <: Param]: Print[(Seq[Seq[P]], Seq[P])] = Print { case (expl, impl) =>
    p(r(expl),
      if (impl.isEmpty) p()
      else p("(implicit ", r(impl, ", "), ")"))
  }
  implicit val printParamTypeOpt: Print[Option[ParamType]] = Print { _.map { t => p(": ", t) }.getOrElse(p()) }
  implicit val printTypeOpt: Print[Option[Type]] = Print { _.map { t => p(": ", t) }.getOrElse(p()) }
  implicit val printTermNameOpt: Print[Option[Term.Name]] = Print { _.map(p(_)).getOrElse(p(")")) }
  implicit val printImportSels: Print[Seq[Import.Selector]] = Print {
    case (t: Import.Selector.Name) :: Nil     => p(t)
    case (t: Import.Selector.Wildcard) :: Nil => p(t)
    case sels                                 => p("{ ", r(sels, ", "), " }")
  }
}

object Test extends App {
  val source = Source.File("/Users/Den/Proj/scala/v2.11.0/src/compiler/scala/tools/nsc/typechecker/Typers.scala")
  val tree = (new SourceParser(source)).parse()
  val out = new java.io.PrintWriter("out.scala")
  try out.print(tree.print)
  finally out.close()
}
