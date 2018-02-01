package flags

package object p {
  private lazy val x = 1
  protected implicit var y: Int = 2
  def z = 3
  abstract class C[+T, -U, V](x: T, y: U, z: V) {
    def this() = this(???, ???, ???)
    def w: Int
  }
  type T = Int
  type U <: Int
  type V >: Int
  case object X
  final class Y
  sealed trait Z
}
