package scala.meta.testkit

private[testkit] object CompatParCollections {
  val Converters = {
    import Compat._
    {
      import scala.collection.parallel._
      CollectionConverters
    }
  }

  object Compat {
    object CollectionConverters
  }
}
