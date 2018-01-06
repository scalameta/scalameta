package scala.meta.contrib.instances

import scala.meta.contrib._
import scala.meta._

trait ReplaceModsInstances {
  implicit val replaceClassMods: Replace[Defn.Class, Mod] =
    Replace((a, bs) => a.copy(mods = bs))

  implicit val replaceTraitMods: Replace[Defn.Trait, Mod] =
    Replace((a, bs) => a.copy(mods = bs))

  implicit val replaceObjectMods: Replace[Defn.Object, Mod] =
    Replace((a, bs) => a.copy(mods = bs))

  implicit val replaceDefMods: Replace[Defn.Def, Mod] =
    Replace((a, bs) => a.copy(mods = bs))

  implicit val replaceValMods: Replace[Defn.Val, Mod] =
    Replace((a, bs) => a.copy(mods = bs))

  implicit val replaceVarMods: Replace[Defn.Var, Mod] =
    Replace((a, bs) => a.copy(mods = bs))

  implicit val replaceTypeMod: Replace[Defn.Type, Mod] =
    Replace((a, bs) => a.copy(mods = bs))

  implicit val replaceTypeParamMod: Replace[Type.Param, Mod] =
    Replace((a, bs) => a.copy(mods = bs))

  implicit val replaceTermParamMod: Replace[Term.Param, Mod] =
    Replace((a, bs) => a.copy(mods = bs))

  implicit val replaceDeclDefMod: Replace[Decl.Def, Mod] =
    Replace((a, bs) => a.copy(mods = bs))

  implicit val replaceDeclVarMod: Replace[Decl.Var, Mod] =
    Replace((a, bs) => a.copy(mods = bs))

  implicit val replaceDeclValMod: Replace[Decl.Val, Mod] =
    Replace((a, bs) => a.copy(mods = bs))

  implicit val replaceDeclTypeMod: Replace[Decl.Type, Mod] =
    Replace((a, bs) => a.copy(mods = bs))
}

object ReplaceModsInstances extends ReplaceModsInstances
