import scala.meta._

object Main {
  def main(args: Array[String]): Unit = {
    // Test that loading up the mirror will not fail.
    implicit val mirror = Mirror()
    org.scalameta.logger.elem(mirror.database, mirror.sources)
    assert(mirror.sources.length == 2, s"${mirror.sources.length} is not 2!")
  }
}
