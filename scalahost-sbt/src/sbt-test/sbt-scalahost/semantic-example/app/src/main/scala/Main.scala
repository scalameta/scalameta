import scala.meta._

object Main {
  def main(args: Array[String]): Unit = {
    // Test that loading up the mirror will not fail.
    implicit val mirror = Mirror()
    println(mirror.database)
  }
}
