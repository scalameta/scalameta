package scala.meta.contrib.instances

import scala.collection.immutable.Seq
import scala.meta.Mod
import scala.meta.contrib.Extract

trait ExtractAnnotationInstances {
  implicit def extractAnnotationsFromMods[A](
      implicit ev: Extract[A, Seq[Mod]]): Extract[A, Seq[Mod.Annot]] =
    new Extract[A, Seq[Mod.Annot]] {
      override def extract(a: A): Seq[Mod.Annot] =
        ev.extract(a).collect { case d: Mod.Annot => d }
    }
}

object ExtractAnnotationInstances
