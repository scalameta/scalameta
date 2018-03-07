package scala.meta.cli

import java.io._
import org.langmeta._
import scala.meta.internal.metacp._
import scala.meta.metacp._

object Metacp {
  def main(args: Array[String]): Unit = {
    sys.exit(process(args, Reporter()))
  }

  @deprecated("Use `process(Settings.parse(args.toList).get, Reporter())`.", "3.5.0")
  def process(args: Array[String]): Int = {
    process(args, Reporter())
  }

  @deprecated("Use `process(Settings.parse(args.toList).get, Reporter(out, err))`.", "3.5.0")
  def process(args: Array[String], out: PrintStream, err: PrintStream): Int = {
    process(args, Reporter().withOut(out).withErr(err))
  }

  private def process(args: Array[String], reporter: Reporter): Int = {
    Settings.parse(args.toList) match {
      case Some(settings) =>
        process(settings, reporter) match {
          case Some(mclasspath) =>
            reporter.out.println(mclasspath.shallow.mkString(File.pathSeparator))
            0
          case None =>
            1
        }
      case None =>
        1
    }
  }

  def process(settings: Settings, reporter: Reporter): Option[Classpath] = {
    val main = new Main(settings, reporter)
    main.process()
  }
}
