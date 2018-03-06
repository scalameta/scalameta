package scala.meta.internal.metacp

import java.io.PrintStream

import org.langmeta.io.AbsolutePath
import org.langmeta.io.Classpath

import scala.meta.cli.Metacp

final case class Settings(
    out: PrintStream = System.out,
    err: PrintStream = System.err,
    classpath: Classpath = Classpath(Nil),
    d: AbsolutePath = Metacp.defaultCacheDir,
    directories: DirectoryOutput = DirectoryOutput.TempDirectory
) {
  def withClasspath(e: String) =
    Classpath(classpath.shallow ++ Classpath(e).shallow)
}

sealed trait DirectoryOutput
object DirectoryOutput {
  case object TempDirectory extends DirectoryOutput
  case object InPlace extends DirectoryOutput
}

object Settings {
  def parse(args: List[String], out: PrintStream, err: PrintStream): Option[Settings] = {
    def loop(settings: Settings, allowOptions: Boolean, args: List[String]): Option[Settings] = {
      args match {
        case "--" +: rest =>
          loop(settings, false, rest)
        case "-cp" +: cp +: rest if allowOptions =>
          val cps1 = settings.withClasspath(cp)
          loop(settings.copy(classpath = cps1), true, rest)
        case "-d" +: d +: rest if allowOptions =>
          loop(settings.copy(d = AbsolutePath(d)), true, rest)
        case "-dirs" +: d +: rest if allowOptions =>
          loop(settings.copy(d = AbsolutePath(d)), true, rest)
        case "--dirs-in-place" :: rest =>
          loop(settings.copy(directories = DirectoryOutput.InPlace), true, rest)
        case "--dirs-in-tmp" :: rest =>
          loop(settings.copy(directories = DirectoryOutput.TempDirectory), true, rest)
        case flag +: _ if allowOptions && flag.startsWith("-") =>
          err.println(s"unknown flag $flag")
          None
        case cp +: rest =>
          val cps1 = settings.withClasspath(cp)
          loop(settings.copy(classpath = cps1), true, rest)
        case Nil =>
          Some(settings)
      }
    }
    loop(Settings(out = out, err = err), allowOptions = true, args)
  }
}
