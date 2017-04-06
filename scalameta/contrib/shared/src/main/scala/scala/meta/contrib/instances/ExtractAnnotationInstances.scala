package scala.meta.contrib.instances

import scala.meta._
import scala.meta.contrib._

trait ExtractAnnotationInstances {
  implicit def extractAnnotationsFromMods[A](implicit ev: Extract[A, Mod]): Extract[A, Mod.Annot] =
    Extract(a => ev.extract(a).collect { case m: Mod.Annot => m })
}

object ExtractAnnotationInstances extends ExtractAnnotationInstances
