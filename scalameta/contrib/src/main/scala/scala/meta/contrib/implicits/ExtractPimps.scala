package scala.meta.contrib.implicits

import scala.meta._
import scala.meta.contrib.Extract
import scala.collection.immutable.Seq

trait ExtractPimps {

  // Helper types since Seq has to be immutable
  type Stats = Seq[Stat]
  type Defs = Seq[Defn.Def]
  type Vals = Seq[Defn.Val]
  type Vars = Seq[Defn.Var]
  type Traits = Seq[Defn.Trait]
  type Objects = Seq[Defn.Object]
  type Classes = Seq[Defn.Class]
  type Types = Seq[Defn.Type]
  type Mods = Seq[Mod]
  type Annotations = Seq[Mod.Annot]

  implicit class XtensionExtractors[A](a: A) {

    /**
      * Logical extraction of B from A.
      *
      * Meaning that this is supposed to replicate what the use thinks should happen.
      * Not the actual class representation
      *
      * eg. Extract[Defn.Class, Seq[Stat]]
      *
      * is actually extracting the stats from the Template, which is a child of Defn.Class.
      */
    def extract[B](implicit ev: Extract[A, B]): B = ev.extract(a)
  }
}
