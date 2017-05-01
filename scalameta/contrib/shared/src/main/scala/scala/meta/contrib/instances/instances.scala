package scala.meta.contrib.instances

trait instances
    extends ExtractAnnotationInstances
    with ExtractModsInstances
    with ExtractStatInstances
    with ExtractStatSubtypeInstances
    with ReplaceStatInstances
    with ReplaceModsInstances

// Lowercase so it looks like a package object
object instances extends instances
