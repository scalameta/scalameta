import scala.meta._

object Main {
  def main(args: Array[String]): Unit = {
    // Test that loading up the mirror will not fail.
    implicit val mirror = Mirror()
    org.scalameta.logger.elem(mirror.database, mirror.sources)
    assert(mirror.sources.length == 2, s"${mirror.sources.length} is not 2!")
    assert(mirror.database.messages.length == 1, // deprecation warnings are emitted post-typer
           s"""Database.messages.length: ${mirror.database.messages.length}
              |Database.messages: ${mirror.database.messages}
              |Expected length: 1
              |Database:
              |${mirror.database}""".stripMargin)
  }
}
