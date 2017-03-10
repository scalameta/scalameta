package scala.meta.contrib.instances

import scala.meta._
import scala.meta.contrib._

trait ExtractModsInstances {
  implicit val extractClassMods: Extract[Defn.Class, Mods] =
    Extract(_.mods)

  implicit val extractTraitMods: Extract[Defn.Trait, Mods] =
    Extract(_.mods)

  implicit val extractObjectMods: Extract[Defn.Object, Mods] =
    Extract(_.mods)

  implicit val extractDefMods: Extract[Defn.Def, Mods] =
    Extract(_.mods)

  implicit val extractValMods: Extract[Defn.Val, Mods] =
    Extract(_.mods)

  implicit val extractVarMods: Extract[Defn.Var, Mods] =
    Extract(_.mods)
}

object ExtractModsInstances extends ExtractModsInstances
