package scala.meta.testkit

import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.meta._
import scala.util.control.NonFatal

import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicInteger

import geny.Generator

object SyntaxAnalysis {

  /** Run custom analysis on a corpus of [[ScalaFile]].
    *
    * @param corpus         The corpus to run analysis on.
    * @param onParseSuccess callback when file successfully parsed.
    * @param onParseError   callback when file failed to parse. Default does nothing.
    * @tparam T The kind of analysis we want to collect.
    * @return The aggregate sum of all analysis results.
    */
  def run[T](corpus: mutable.Buffer[ScalaFile])(
      onParseSuccess: Source => Seq[T],
      onParseError: (ScalaFile, parsers.Parsed.Error) => Seq[T] =
        (_: ScalaFile, _: parsers.Parsed.Error) => Nil
  ): mutable.Buffer[(ScalaFile, T)] = {
    val results = new CopyOnWriteArrayList[(ScalaFile, T)]
    val counter = new AtomicInteger()
    val errors = new AtomicInteger()
    def analyze(file: ScalaFile): Unit = {
      val n = counter.incrementAndGet()
      if (n % 1000 == 0) {
        println(n)
      }
      try {
        val ts: Seq[T] = file.jFile.parse[Source] match {
          case parsers.Parsed.Success(ast) => onParseSuccess(ast)
          case err: parsers.Parsed.Error => onParseError(file, err)
        }
        ts.foreach { t =>
          results.add(file -> t)
        }
      } catch {
        case _: org.scalameta.invariants.InvariantFailedException => // scala.meta error
        case _: java.nio.charset.MalformedInputException => // scala.meta error
        case _: java.util.NoSuchElementException => // scala.meta error
        case NonFatal(e) =>
          // unexpected errors are printed in the console.
          e.printStackTrace()
          println(e.getClass.getSimpleName)
          val i = errors.incrementAndGet()
          if (i > 10) {
            throw new IllegalStateException(
              "Too many unexpected errors, fix your analysis.")
          }
      }
    }
    corpus.par.foreach(analyze)
    results.asScala
  }

}
