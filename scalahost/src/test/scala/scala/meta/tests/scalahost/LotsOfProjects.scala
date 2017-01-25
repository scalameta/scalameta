package scala.meta.tests
package scalahost

import scala.collection.mutable
import scala.util.Try

import java.util.concurrent.atomic.AtomicInteger
import scala.collection.JavaConverters._

import scala.meta.internal.converters.ConvertException
import org.scalatest.exceptions.TestFailedException

object LotsOfProjects extends ConverterSuite {
  override val parseAsCompilationUnit: Boolean = true

  def err2message(e: Throwable): String = {
    val details =
      e match {
        case e: ConvertException if e.getMessage.startsWith("unsupported") =>
          s"${e.culprit.getClass.getSimpleName}"
        case e: TestFailedException if e.getMessage().startsWith("scalac parse err") =>
          "parse error"
        case e =>
          e.getMessage.lines.take(1).toSeq.mkString
      }
    s"${e.getClass.getSimpleName}: $details"
  }

  def getResults: mutable.Buffer[String] = {
    val files   = ScalaFile.getAll
    val counter = new AtomicInteger()
    val results = new java.util.concurrent.CopyOnWriteArrayList[String]()

    files.toArray.foreach { file =>
      val code = file.read
      val n    = counter.incrementAndGet()
      if (n % 1000 == 0) println(s"$n...")
      Try(getConvertedMetaTree(code)) match {
        // Uncomment to investigate a specific error further.
        //          case scala.util.Failure(e: ConvertException)
        //              if e.culprit.getClass.getSimpleName == "EmptyTree$" =>
        //            println(file.githubUrl)
        //            val culprit = e.culprit.toString.lines.take(1).mkString
        //            e.printStackTrace()
        //            results.add(err2message(e))
        case scala.util.Failure(e) =>
          results.add(err2message(e))
        case _ =>
          results.add("Success")
      }
    }
    results.asScala
  }

  def printResults(results: mutable.Buffer[String]): Unit = {
    val mappedResults =
      results
        .groupBy(x => x)
        .mapValues(_.length)

    mappedResults
      .filter(_._2 > 1)
      .toArray
      .sortBy(_._2)
      .foreach {
        case (k, v) => println(s"$k: $v")
      }
    println(s"Total: ${mappedResults.size}")
  }

  def main(args: Array[String]): Unit = {
    val results = getResults
    printResults(results)
    assert(results.count(_ == "Success") >= 25950) // increment this number as it increases.
  }
}
