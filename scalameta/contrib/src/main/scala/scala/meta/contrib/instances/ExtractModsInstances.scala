package scala.meta.contrib.instances

import scala.collection.immutable.Seq
import scala.meta._
import scala.meta.contrib.Extract

trait ExtractModsInstances {
  implicit val extractClassMods: Extract[Defn.Class, Seq[Mod]] =
    new Extract[Defn.Class, Seq[Mod]] {
      override def extract(c: Defn.Class): Seq[Mod] =
        c.mods
    }

  implicit val extractTraitMods: Extract[Defn.Trait, Seq[Mod]] =
    new Extract[Defn.Trait, Seq[Mod]] {
      override def extract(t: Defn.Trait): Seq[Mod] =
        t.mods
    }

  implicit val extractObjectMods: Extract[Defn.Object, Seq[Mod]] =
    new Extract[Defn.Object, Seq[Mod]] {
      override def extract(o: Defn.Object): Seq[Mod] =
        o.mods
    }

  implicit val extractDefMods: Extract[Defn.Def, Seq[Mod]] = new Extract[Defn.Def, Seq[Mod]] {
    override def extract(d: Defn.Def): Seq[Mod] =
      d.mods
  }

  implicit val extractValMods: Extract[Defn.Val, Seq[Mod]] = new Extract[Defn.Val, Seq[Mod]] {
    override def extract(v: Defn.Val): Seq[Mod] =
      v.mods
  }

  implicit val extractVarMods: Extract[Defn.Var, Seq[Mod]] = new Extract[Defn.Var, Seq[Mod]] {
    override def extract(v: Defn.Var): Seq[Mod] =
      v.mods
  }
}

object ExtractModsInstances extends ExtractModsInstances
