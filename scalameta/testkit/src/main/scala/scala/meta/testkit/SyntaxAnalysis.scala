package scala.meta.testkit

import java.util.concurrent.CopyOnWriteArrayList
import java.util.concurrent.atomic.AtomicInteger
import scala.collection.GenIterable
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.meta._
import scala.util.control.NonFatal

object SyntaxAnalysis {

  /** Run syntactic analysis on a corpus of [[CorpusFile]].
    *
    * @param corpus The corpus to run analysis on. Has type GenIterable
    *               to support both parallel and synchronous collections.
    * @param f      Callback to analyse a single [[CorpusFile]].
    * @tparam T The kind of analysis we want to collect.
    * @return The aggregate sum of all analysis results.
    */
  def run[T](corpus: GenIterable[CorpusFile])(
      f: CorpusFile => List[T]
  ): mutable.Buffer[(CorpusFile, T)] = Phase.run("syntax analysis") {
    val results = new CopyOnWriteArrayList[(CorpusFile, T)]
    val counter = new AtomicInteger()
    val errors  = new AtomicInteger()
    def analyze(file: CorpusFile): Unit = {
      val n = counter.incrementAndGet()
      if (n % 1000 == 0) {
        println(n)
      }
      try {
        f(file).foreach(t => results.add(file -> t))
      } catch {
        // TODO(olafur) investigate these scala.meta errors.
        case _: org.scalameta.UnreachableError                    => // scala.meta error
        case _: org.scalameta.invariants.InvariantFailedException => // scala.meta error
        case _: java.nio.charset.MalformedInputException          => // scala.meta error
        case _: java.util.NoSuchElementException                  => // scala.meta error
        case NonFatal(e)                                          =>
          // unexpected errors are printed in the console.
          println(s"Unexpected error analysing file: $file")
          println(s"Error: ${e.getClass.getName} $e")
          val stack = e.getStackTrace.take(10) // print small stacktrace
          stack.foreach(println)
          val i = errors.incrementAndGet()
          if (i > 10) {
            throw new IllegalStateException(
              "Too many unexpected errors (printed to console), fix your analysis.")
          }
      }
    }
    corpus.foreach(analyze)
    results.asScala
  }

  def onParsed[A](corpus: GenIterable[CorpusFile])(
      f: Source => List[A]): mutable.Buffer[(CorpusFile, A)] =
    SyntaxAnalysis.run[A](corpus)(_.jFile.parse[Source] match {
      case parsers.Parsed.Success(ast: Source) => f(ast)
      case _                                   => Nil
    })
}
