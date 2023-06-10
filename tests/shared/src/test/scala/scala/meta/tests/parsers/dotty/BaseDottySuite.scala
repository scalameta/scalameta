package scala.meta.tests.parsers.dotty

import scala.meta.tests.parsers._
import scala.meta._

trait BaseDottySuite extends ParseSuite {

  protected implicit val dialect: Dialect = dialects.Scala3

  final val anon = meta.Name.Anonymous()
  final val phName = meta.Name.Placeholder()
  final val ctor = Ctor.Primary(Nil, anon, Nil)
  final def ctorp(lp: List[Term.Param] = Nil) = Ctor.Primary(Nil, anon, List(lp))
  final val slf = meta.Self(anon, None)
  final def self(name: String, tpe: String = null) = meta.Self(tname(name), Option(tpe).map(pname))

  final def tname(name: String): Term.Name = Term.Name(name)
  final def tpl(stats: List[Stat]): Template = Template(Nil, Nil, slf, stats)

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
  final def init(name: String): Init = Init(pname(name), anon, Nil)

}
