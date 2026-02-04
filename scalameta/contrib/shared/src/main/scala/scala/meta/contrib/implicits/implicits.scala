package scala.meta.contrib.implicits

trait implicits
    extends Converters
    with Equality
    with ReplaceExtensions
    with ExtractExtensions
    with SetExtensions
    with TreeExtensions

// Lowercase so it looks like a package object
object implicits extends implicits
