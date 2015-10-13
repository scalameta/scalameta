package org.scalameta.debug

// TODO: I can't say that I like this whole business with delayed logs,
// but the situation is more or less forcing me to go for that.
//
// The thing is that logging via show[Attributes] can end up forcing yet unforced typings.
// Forcing can result in arbitrary closures running to really wild effects.
//
// One of the unpleasant things that can happen are calls to toMmember,
// which need to look into member caches, and there are situations when those caches
// are locked to avoid cache coherence issues (e.g. if we're converting multiple
// compilation units, we can't allow anyone to call toMmember until everything is converted
// and indexed).
//
// Craziness of this entire thing is reaching dangerous levels, approaching scala.reflect internals,
// and I definitely don't like that.

object Debug {
  private var areSensitiveLogsDelayed = false
  private var delayedCounter = 0
  private var delayedLogs = List[() => Unit]()

  def debug = sys.props("debug.debug") != null

  def verbose = sys.props("verbose.debug") != null

  def delayingSensitiveLogs[T](op: => T): T = {
    if (debug) println(s"delaying sensitive logs at $areSensitiveLogsDelayed")
    val old = areSensitiveLogsDelayed
    areSensitiveLogsDelayed = true
    val result = {
      try op
      finally areSensitiveLogsDelayed = old
    }
    if (!old) {
      if (debug) println(s"releasing sensitive logs")
      if (delayedLogs.nonEmpty) println("")
      delayedLogs.foreach(_.apply())
      delayedCounter = 0
    }
    result
  }

  def log(op: => Unit): Unit = {
    op
  }

  def sensitiveLog(op: => Unit): Unit = {
    if (areSensitiveLogsDelayed) {
      if (debug) println("sensitive log is about to be delayed")
      delayedCounter = delayedCounter + 1
      delayedLogs = delayedLogs :+ { () =>
        Console.out.print(s"($delayedCounter)")
        op
      }
      println(s"($delayedCounter) sensitive log is delayed to avoid premature forcing of typings")
    } else {
      op
    }
  }
}