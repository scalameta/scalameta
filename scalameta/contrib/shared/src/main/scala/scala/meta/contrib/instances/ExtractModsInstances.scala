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
}

object ExtractModsInstances extends ExtractModsInstances
