package scala.meta.testkit

object Phase {
  def run[T](phase: String)(f: => T): T = {
    def startPhase(phase: String): Unit = println(s"===> Starting $phase...")
    def endPhase(phase: String): Unit = println(s"===> $phase completed!")
    startPhase(phase)
    val result = f
    endPhase(phase.capitalize)
    result
  }
}
