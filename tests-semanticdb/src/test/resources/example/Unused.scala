package example

// scalafix's RemoveUnused matches these warnings to trees by position
class Unused/*<=example.Unused#*/(ctorParam/*<=example.Unused#`<init>`().(ctorParam)*//*<=example.Unused#ctorParam.*/: Int/*=>scala.Int#*/) {

  private object PrivateObject/*<=example.Unused#PrivateObject.*/
  private class PrivateClass/*<=example.Unused#PrivateClass#*/
  private trait PrivateTrait/*<=example.Unused#PrivateTrait#*/
  private type PrivateType/*<=example.Unused#PrivateType#*/ = Int/*=>scala.Int#*/
  private def privateDef/*<=example.Unused#privateDef().*/ = 1
  private val privateVal/*<=example.Unused#privateVal.*/ = 1
  private var privateVar/*<=example.Unused#privateVar().*/ = 1
  @deprecated/*=>scala.deprecated#*//*=>scala.deprecated#`<init>`().*/("", "")
  private def annotatedDef/*<=example.Unused#annotatedDef().*/ = 1
  private final case class PrivateCaseClass/*<=example.Unused#PrivateCaseClass#*/()

  def locals/*<=example.Unused#locals().*/(): Unit/*=>scala.Unit#*/ = {
    object LocalObject/*<=local0*/
    class LocalClass/*<=local1*/
    trait LocalTrait/*<=local3*/
    type LocalType/*<=local4*/ = Int/*=>scala.Int#*/
    def localDef/*<=local5*/ = 1
    val localVal/*<=local6*/ = 1
    var localVar/*<=local7*/ = 1
    val (tupleA/*<=local8*/, tupleB/*<=local9*/) = (1, 2)
  }

  def params/*<=example.Unused#params().*/(param/*<=example.Unused#params().(param)*/: Int/*=>scala.Int#*/)(implicit implicitParam/*<=example.Unused#params().(implicitParam)*/: Int/*=>scala.Int#*/): Unit/*=>scala.Unit#*/ = println/*=>scala.Predef.println().*/()
  def implicitAlone/*<=example.Unused#implicitAlone().*/(implicit alone/*<=example.Unused#implicitAlone().(alone)*/: Int/*=>scala.Int#*/): Unit/*=>scala.Unit#*/ = println/*=>scala.Predef.println().*/()
  def implicitPair/*<=example.Unused#implicitPair().*/(implicit first/*<=example.Unused#implicitPair().(first)*/: Int/*=>scala.Int#*/, second/*<=example.Unused#implicitPair().(second)*/: String/*=>scala.Predef.String#*/): Unit/*=>scala.Unit#*/ = println/*=>scala.Predef.println().*/()

  def patterns/*<=example.Unused#patterns().*/(x/*<=example.Unused#patterns().(x)*/: Option/*=>scala.Option#*/[Int/*=>scala.Int#*/]): Unit/*=>scala.Unit#*/ = x/*=>example.Unused#patterns().(x)*/ match {
    case Some/*=>scala.Some.*/(patVar/*<=local10*/) => ()
    case _ => ()
  }

}
