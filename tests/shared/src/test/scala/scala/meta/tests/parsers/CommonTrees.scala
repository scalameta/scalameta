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
    def apply(): Self = Self(Name.Anonymous(), None)
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
    def apply(): Template = Template(Nil, Nil, EmptySelf(), Nil)
    def unapply(tree: Tree): Boolean = tree match {
      case Template(Nil, Nil, EmptySelf(), Nil) => true
      case _ => false
    }
  }

  final val anon = meta.Name.Anonymous()
  final val phName = meta.Name.Placeholder()
  final val ctor = EmptyCtor()
  final def ctorp(lp: List[Term.Param] = Nil) = Ctor.Primary(Nil, anon, List(lp))
  final val slf = meta.Self(anon, None)
  final def self(name: String, tpe: String = null) = meta.Self(tname(name), Option(tpe).map(pname))

  final def tname(name: String): Term.Name = Term.Name(name)
  final def tpl(inits: List[Init], stats: List[Stat]): Template =
    Template(Nil, inits, slf, stats)
  final def tpl(stats: List[Stat]): Template = tpl(Nil, stats)

  final def tparam(mods: List[Mod], name: String, tpe: Option[Type] = None): Term.Param = {
    val nameTree = name match {
      case "" => anon
      case "_" => phName
      case _ => Term.Name(name)
    }
    Term.Param(mods, nameTree, tpe, None)
  }
  final def tparam(name: String, tpe: Option[Type]): Term.Param =
    tparam(Nil, name, tpe)
  final def tparam(mods: List[Mod], name: String, tpe: Type): Term.Param =
    tparam(mods, name, Option(tpe))
  final def tparam(name: String, tpe: Type): Term.Param =
    tparam(Nil, name, tpe)
  final def tparam(mods: List[Mod], name: String, tpe: String): Term.Param =
    tparam(mods, name, Option(tpe).map(pname))
  final def tparam(name: String, tpe: String): Term.Param =
    tparam(Nil, name, tpe)

  final def tparamval(name: String, tpe: String) =
    tparam(List(Mod.ValParam()), name, tpe)
  final def tparamInline(name: String, tpe: String) =
    tparam(List(Mod.Inline()), name, tpe)
  final def tparamUsing(name: String, tpe: String) =
    tparam(List(Mod.Using()), name, tpe)

  final def pname(name: String): Type.Name = Type.Name(name)
  final def pparam(s: String): Type.Param =
    Type.Param(Nil, Type.Name(s), Nil, noBounds, Nil, Nil)

  final val noBounds = Type.Bounds(None, None)
  final def lowBound(bound: Type) = Type.Bounds(Some(bound), None)

  final def bool(v: Boolean) = Lit.Boolean(v)
  final def int(v: Int) = Lit.Int(v)
  final def str(v: String) = Lit.String(v)
  final def init(name: String, args: List[Term.ArgClause] = Nil): Init =
    Init(pname(name), anon, args)

}
