package scala.meta.tests
package parsers

import scala.meta._

import scala.language.implicitConversions

object CommonTrees {
  trait LowPriorityDefinitions {
    final def nameOrCapset[A](name: String, fname: String => A, fcapset: String => A): A = {
      val withoutSuffix = name.stripSuffix("^")
      if (name eq withoutSuffix) fname(name) else fcapset(withoutSuffix)
    }

    final def tname(name: String): Term.Name = Term.Name(name)
    final def tnameComments(name: String)(begComment: String*)(endComment: String*): Term.Name =
      Term.Name.createWithComments(name, begComment = begComment, endComment = endComment)
    final def tnameCapset(name: String) = Term.CapSetName(name)
    final def tnameOrCapset(name: String): meta.Name with Term.Ref =
      nameOrCapset(name, tname, tnameCapset)
    implicit def implicitStringToTerm(obj: String): Term.Name = tname(obj)

    final def pname(name: String): Type.Name = Type.Name(name)
    final def pnameCapset(name: String) = Type.CapSetName(name)
    final def pnameOrCapset(name: String): meta.Name with Type.Ref =
      nameOrCapset(name, pname, pnameCapset)
    implicit def implicitStringToType(obj: String): Type.Name = pname(obj)

    final def comment(str: String): Tree.Comment = str :: Nil
    final def comment(vals: String*): Tree.Comment = vals
    implicit def implicitStringToComment(obj: String): Tree.Comment = comment(obj)
    implicit def implicitSeqStringToComment(objs: Seq[String]): Tree.Comment = Tree
      .Comment(parts = objs.map(Lit.String.apply).toList)

    final def comments(vals: Tree.Comment*): Option[Tree.Comments] =
      if (vals.isEmpty) None else Some(Tree.Comments(vals.toList))
    final def comments(str: String, strs: String*): Option[Tree.Comments] = str +: strs
    implicit def implicitCommentToOptionComments(obj: Tree.Comment): Option[Tree.Comments] =
      comments(obj)
    implicit def implicitSeqStringToOptionComments(objs: Seq[String]): Option[Tree.Comments] =
      comments(objs.map(comment): _*)

  }
}

trait CommonTrees extends CommonTrees.LowPriorityDefinitions {
  object Nothing {
    def unapply(tree: Tree): Boolean = tree match {
      case Type.Name("Nothing") => true
      case _ => false
    }
  }

  object Any {
    def unapply(tree: Tree): Boolean = tree match {
      case Type.Name("Any") => true
      case _ => false
    }
  }

  object EmptySelf {
    def apply(): Self = Self(anon, None)
    def unapply(tree: Tree): Boolean = tree match {
      case Self(Name.Anonymous(), None) => true
      case _ => false
    }
  }

  object EmptyCtor {
    def apply() = Ctor.Primary(Nil, anon, Seq.empty[Term.ParamClause])
    def unapply(tree: Tree): Boolean = tree match {
      case Ctor.Primary(Nil, Name.Anonymous(), Nil) => true
      case _ => false
    }
  }

  object EmptyTemplate {
    def apply(): Template = tpl()
    def unapply(tree: Tree): Boolean = tree match {
      case Template(Nil, Nil, EmptySelf(), Nil) => true
      case _ => false
    }
  }

  final val anon = meta.Name.Anonymous()
  final val phName = meta.Name.Placeholder()
  final val ctor = EmptyCtor()
  final def ctorp(lp: Term.Param*): Ctor.Primary = ctorp(lp.toList)
  final def ctorp(lp: List[Term.Param], lps: List[Term.Param]*): Ctor.Primary = Ctor
    .Primary(Nil, anon, lp :: lps.toList)
  final def ctorp(mod: Mod.ParamsType, lp: Term.Param*): Ctor.Primary = Ctor
    .Primary(Nil, anon, Term.ParamClause(lp.toList, Option(mod)) :: Nil)
  final val slf = meta.Self(anon, None)
  final def self(name: meta.Name, tpe: Option[Type] = None): meta.Self = meta.Self(name, tpe)
  final def self(name: meta.Name, tpe: Type): meta.Self = self(name, Option(tpe))

  implicit def implicitStringsToTerms(obj: List[String]): List[Term.Name] = obj.map(tname)
  implicit def implicitStringToTermOpt(obj: Option[String]): Option[Term.Name] = obj.map(tname)

  final def nameOr(name: String, f: String => meta.Name): meta.Name = name match {
    case "" => anon
    case "_" => phName
    case _ => f(name)
  }
  final def mname(name: String): meta.Name = nameOr(name, tname)
  implicit def implicitStringToName(obj: String): meta.Name = mname(obj)
  implicit def implicitStringsToNames(obj: List[String]): List[meta.Name] = obj.map(mname)
  implicit def implicitStringToNameOpt(obj: Option[String]): Option[meta.Name] = obj.map(mname)

  final def tplNoBody(inits: List[Init]): Template = Template(Nil, inits, tplBody())
  final def tplNoBody(inits: Init*): Template = tplNoBody(inits.toList)

  final def tpl(inits: List[Init], body: Template.Body): Template = Template(Nil, inits, body)
  final def tpl(inits: List[Init], stats: List[Stat]): Template = tpl(inits, tplBody(stats: _*))
  final def tpl(inits: List[Init], self: meta.Self, stats: Stat*): Template =
    tpl(inits, tplBody(self, stats: _*))
  final def tpl(stats: List[Stat]): Template = tpl(Nil, stats)
  final def tpl(stats: Stat*): Template = tpl(stats.toList)
  final def tpl(body: Template.Body): Template = tpl(Nil, body)
  final def tpl(self: meta.Self, stats: Stat*): Template = tpl(Nil, self, stats: _*)

  final def tplBody(selfOpt: Option[meta.Self], stats: Stat*): Template.Body = Template
    .Body(selfOpt, stats.toList)
  final def tplBody(self: meta.Self, stats: Stat*): Template.Body = tplBody(Option(self), stats: _*)
  final def tplBody(name: String, stats: Stat*): Template.Body = tplBody(Some(self(name)), stats: _*)
  final def tplBody(stats: Stat*): Template.Body = tplBody(None, stats: _*)

  final def tparam(mods: List[Mod], name: String, tpe: Option[Type] = None): Term.Param = Term
    .Param(mods, mname(name), tpe, None)
  final def tparam(name: String, tpe: Option[Type]): Term.Param = tparam(Nil, name, tpe)
  final def tparam(mods: List[Mod], name: String, tpe: Type): Term.Param =
    tparam(mods, name, Option(tpe))
  final def tparam(name: String, tpe: Type): Term.Param = tparam(Nil, name, tpe)
  final def tparam(name: String): Term.Param = tparam(Nil, name)

  final def tparamval(name: String, tpe: Type) = tparam(List(Mod.ValParam()), name, tpe)
  final def tparamInline(name: String, tpe: Type) = tparam(List(Mod.Inline()), name, tpe)
  final def tparamUsing(name: String, tpe: Type) = tparam(List(Mod.Using()), name, tpe)

  final def tinfix(lt: Term, op: String, ta: List[Type], rt: Term*): Term.ApplyInfix = Term
    .ApplyInfix(lt, tname(op), ta, rt.toList)
  final def tinfix(lt: Term, op: String, rt: Term*): Term.ApplyInfix = tinfix(lt, op, Nil, rt: _*)
  final def tpostfix(lt: Term, op: String): Term.SelectPostfix = Term.SelectPostfix(lt, op)
  final def tselect(lt: Term, op: String, ops: String*): Term.Select = ops
    .foldLeft(Term.Select(lt, op)) { case (res, op) => Term.Select(res, op) }
  final def tapply(fun: Term, args: Term*): Term.Apply = Term.Apply(fun, args.toList)
  final def tapplyUsing(fun: Term, args: Term*): Term.Apply = Term
    .Apply(fun, Term.ArgClause(args.toList, Some(Mod.Using())))
  final def tapplytype(fun: Term, args: Type*) = Term.ApplyType(fun, args.toList)
  final def tmatch(mods: Mod*)(lt: Term, cases: Case*): Term.Match = Term
    .Match(lt, cases.toList, mods.toList)
  final def tmatch(lt: Term, cases: Case*): Term.Match = tmatch()(lt, cases: _*)
  final def tselectmatch(lt: Term, cases: Case*): Term.SelectMatch = Term
    .SelectMatch(lt, cases.toList)
  final def tfunc(params: Term.Param*)(body: Term): Term.Function = Term
    .Function(params.toList, body)
  final def tctxfunc(params: Term.Param*)(body: Term): Term.ContextFunction = Term
    .ContextFunction(params.toList, body)
  final def tpolyfunc(params: Type.Param*)(body: Term) = Term.PolyFunction(params.toList, body)

  implicit def implicitStringsToTypes(obj: List[String]): List[Type.Name] = obj.map(pname)
  implicit def implicitStringToTypeOpt(obj: Option[String]): Option[Type.Name] = obj.map(pname)

  final def pparam(s: String): Type.Param = pparam(s, noBounds)
  final def pparam(s: String, bounds: Type.Bounds): Type.Param = pparam(Nil, s, bounds = bounds)
  final def pparam(
      mods: List[Mod],
      s: String,
      params: List[Type.Param] = Nil,
      bounds: Type.Bounds = noBounds
  ): Type.Param = {
    val nameTree = nameOr(s, pnameOrCapset)
    Type.Param(mods, nameTree, params, bounds)
  }

  final def pinfix(lt: Type, op: String, rt: Type): Type.ApplyInfix = Type.ApplyInfix(lt, op, rt)
  final def pselect(lt: Term.Ref, op: String, ops: String*): Type.Select = ops match {
    case Seq() => Type.Select(lt, op)
    case ops :+ last => Type.Select(tselect(lt, op, ops: _*), last)
  }
  final def papply(tpe: Type, args: Type*): Type.Apply = Type.Apply(tpe, args.toList)
  final def pfunc(params: Type.FuncParamClause, res: Type): Type.Function = Type
    .Function(params, res)
  final def pfunc(param: Type*)(res: Type): Type.Function = pfunc(param.toList, res)
  final def pctxfunc(param: Type*)(res: Type): Type.ContextFunction = Type
    .ContextFunction(param.toList, res)
  final def purefunc(param: Type*)(res: Type): Type.PureFunction = Type
    .PureFunction(param.toList, res)
  final def purectxfunc(param: Type*)(res: Type): Type.PureContextFunction = Type
    .PureContextFunction(param.toList, res)
  final def ppolyfunc(params: Type.Param*)(body: Type) = Type.PolyFunction(params.toList, body)

  final def pcap(tpe: Type, caps: Term.Ref*): Type.Capturing = Type.Capturing(tpe, caps.toList)
  final def pcap(tpe: String, caps: String*): Type.Capturing = pcap(tpe, caps.map(tnameOrCapset): _*)

  final def tpc(tp: Term.Param*): Term.ParamClause = tpc(null, tp: _*)
  final def tpc(mod: Mod.ParamsType, tp: Term.Param*): Term.ParamClause = Term
    .ParamClause(tp.toList, Option(mod))
  final val noTpc = tpc()

  final def ppc(tp: Type.Param*): Type.ParamClause = Type.ParamClause(tp.toList)
  final val noPpc = ppc()

  final def pcg(tp: Type.ParamClause, pp: Term.ParamClause*): Member.ParamClauseGroup = Member
    .ParamClauseGroup(tp, pp.toList)
  final def pcg(pp: Term.ParamClause*): Member.ParamClauseGroup = pcg(Type.ParamClause(Nil), pp: _*)

  final def patvar(name: String): Pat.Var = Pat.Var(name)
  implicit def implicitStringToPatVar(obj: String): Pat.Var = patvar(obj)

  final def patinfix(lt: Pat, op: String, rt: Pat*): Pat.ExtractInfix = Pat
    .ExtractInfix(lt, tname(op), rt.toList)
  final def patextract(fun: Term, args: Pat*) = Pat.Extract(fun, args.toList)
  final val patwildcard = Pat.Wildcard()

  final val noBounds = Type.Bounds.empty
  final def loBound(bound: Type): Type.Bounds = Type.Bounds(Some(bound), None)
  final def hiBound(bound: Type): Type.Bounds = Type.Bounds(None, Some(bound))
  final def bounds(
      lo: Type = null,
      hi: Type = null,
      cb: List[Type] = Nil,
      vb: List[Type] = Nil
  ): Type.Bounds = Type.Bounds(Option(lo), Option(hi), cb, vb)

  final def pwildcard(bounds: Type.Bounds): Type.Wildcard = Type.Wildcard(bounds)
  final val pwildcard: Type.Wildcard = pwildcard(noBounds)

  final def lit() = Lit.Unit()
  final def bool(v: Boolean) = Lit.Boolean(v)
  final def lit(v: Boolean) = Lit.Boolean(v)
  final def int(v: Int) = Lit.Int(v)
  final def lit(v: Int) = Lit.Int(v)
  final def lit(v: Long) = Lit.Long(v)
  final def dbl(v: String) = Lit.Double(v)
  final def lit(v: Double) = Lit.Double(v)
  final def flt(v: String) = Lit.Float(v)
  final def lit(v: Float) = Lit.Float(v)
  final def str(v: String) = Lit.String(v)
  final def lit(v: String) = str(v)
  final def lit(v: Char) = Lit.Char(v)
  final def sym(v: String) = lit(Symbol(v))
  final def lit(v: Symbol) = Lit.Symbol(v)
  final def init(tpe: Type, args: Term.ArgClause*): Init = Init(tpe, anon, args.toList)
  final def blk(stats: Stat*): Term.Block = Term.Block(stats.toList)

  final def stats(vals: Stat*): Stat.Block = Stat.Block(vals.toList)

}
