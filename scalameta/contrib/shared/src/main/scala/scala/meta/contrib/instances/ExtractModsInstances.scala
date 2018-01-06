package scala.meta.contrib.instances

import scala.meta._
import scala.meta.contrib._

trait ExtractModsInstances {
  implicit val extractClassMods: Extract[Defn.Class, Mod] =
    Extract(_.mods)

  implicit val extractTraitMods: Extract[Defn.Trait, Mod] =
    Extract(_.mods)

  implicit val extractObjectMods: Extract[Defn.Object, Mod] =
    Extract(_.mods)

  implicit val extractDefMods: Extract[Defn.Def, Mod] =
    Extract(_.mods)

  implicit val extractValMods: Extract[Defn.Val, Mod] =
    Extract(_.mods)

  implicit val extractVarMods: Extract[Defn.Var, Mod] =
    Extract(_.mods)

  implicit val extractTypeMod: Extract[Defn.Type, Mod] =
    Extract(_.mods)

  implicit val extractTypeParamMod: Extract[Type.Param, Mod] =
    Extract(_.mods)

  implicit val extractTermParamMod: Extract[Term.Param, Mod] =
    Extract(_.mods)

  implicit val extractDeclDefMod: Extract[Decl.Def, Mod] =
    Extract(_.mods)

  implicit val extractDeclVarMod: Extract[Decl.Var, Mod] =
    Extract(_.mods)

  implicit val extractDeclValMod: Extract[Decl.Val, Mod] =
    Extract(_.mods)

  implicit val extractDeclTypeMod: Extract[Decl.Type, Mod] =
    Extract(_.mods)
}

object ExtractModsInstances extends ExtractModsInstances
