package example

// scalafix's RemoveUnused matches these warnings to trees by position
class Unused(ctorParam: Int) {

  private object PrivateObject
  private class PrivateClass
  private trait PrivateTrait
  private type PrivateType = Int
  private def privateDef = 1
  private val privateVal = 1
  private var privateVar = 1
  @deprecated("", "")
  private def annotatedDef = 1
  private final case class PrivateCaseClass()

  def locals(): Unit = {
    object LocalObject
    class LocalClass
    trait LocalTrait
    type LocalType = Int
    def localDef = 1
    val localVal = 1
    var localVar = 1
    val (tupleA, tupleB) = (1, 2)
  }

  def params(param: Int)(implicit implicitParam: Int): Unit = println()
  def implicitAlone(implicit alone: Int): Unit = println()
  def implicitPair(implicit first: Int, second: String): Unit = println()

  def patterns(x: Option[Int]): Unit = x match {
    case Some(patVar) => ()
    case _ => ()
  }

}
