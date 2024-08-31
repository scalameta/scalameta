package scala.meta.tests
package parsers

import scala.meta._

trait CommonTrees {
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
  final def self(name: String, tpe: Option[Type] = None): meta.Self = meta.Self(mname(name), tpe)
  final def self(name: String, tpe: Type): meta.Self = self(name, Option(tpe))
  final def self(name: String, tpe: String): meta.Self = self(name, Option(tpe).map(pname))

  final def tname(name: String): Term.Name = Term.Name(name)
  final def mname(name: String): meta.Name = name match {
    case "" => anon
    case "_" => phName
    case _ => tname(name)
  }

  final def tplNoBody(inits: List[Init]): Template = Template(Nil, inits, tplBody())
  final def tplNoBody(inits: Init*): Template = tplNoBody(inits.toList)

  final def tpl(inits: List[Init], body: Template.Body): Template = Template(Nil, inits, body)
  final def tpl(inits: List[Init], stats: List[Stat]): Template = tpl(inits, tplBody(stats: _*))
  final def tpl(stats: List[Stat]): Template = tpl(Nil, stats)
  final def tpl(stats: Stat*): Template = tpl(stats.toList)

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
  final def tparam(mods: List[Mod], name: String, tpe: String): Term.Param =
    tparam(mods, name, Option(tpe).map(pname))
  final def tparam(name: String, tpe: String): Term.Param = tparam(Nil, name, tpe)
  final def tparam(name: String): Term.Param = tparam(Nil, name)

  final def tparamval(name: String, tpe: String) = tparam(List(Mod.ValParam()), name, tpe)
  final def tparamInline(name: String, tpe: String) = tparam(List(Mod.Inline()), name, tpe)
  final def tparamUsing(name: String, tpe: String) = tparam(List(Mod.Using()), name, tpe)

  final def pname(name: String): Type.Name = Type.Name(name)
  final def pparam(s: String): Type.Param = pparam(s, noBounds)
  final def pparam(s: String, bounds: Type.Bounds): Type.Param = pparam(Nil, s, bounds)
  final def pparam(
      mods: List[Mod],
      s: String,
      bounds: Type.Bounds = noBounds,
      vb: List[Type] = Nil,
      cb: List[Type] = Nil
  ): Type.Param = {
    val nameTree = s match {
      case "" => anon
      case "_" => phName
      case _ => pname(s)
    }
    Type.Param(mods, nameTree, Nil, bounds, vb, cb)
  }

  final val noBounds = Type.Bounds(None, None)
  final def loBound(bound: Type): Type.Bounds = Type.Bounds(Some(bound), None)
  final def loBound(bound: String): Type.Bounds = loBound(pname(bound))
  final def hiBound(bound: Type): Type.Bounds = Type.Bounds(None, Some(bound))
  final def hiBound(bound: String): Type.Bounds = hiBound(pname(bound))
  final def bounds(lo: String = null, hi: String = null): Type.Bounds = Type
    .Bounds(Option(lo).map(pname), Option(hi).map(pname))

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
  final def init(name: String, arg: Term.ArgClause, args: Term.ArgClause*): Init =
    init(name, arg :: args.toList)
  final def init(name: String, args: List[Term.ArgClause] = Nil): Init =
    Init(pname(name), anon, args)
  final def blk(stats: List[Stat]): Term.Block = Term.Block(stats)
  final def blk(stats: Stat*): Term.Block = blk(stats.toList)

  final def stats(vals: Stat*): Stat.Block = Stat.Block(vals.toList)

}
