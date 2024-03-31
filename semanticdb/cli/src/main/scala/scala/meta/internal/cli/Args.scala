package scala.meta.internal.cli

import org.scalameta.internal.ScalaCompat.EOL

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file._

object Args {
  def expand(args: Array[String]): List[String] = args.toList.flatMap { arg =>
    if (arg.startsWith("@")) {
      val argPath = Paths.get(arg.substring(1))
      val argText = new String(Files.readAllBytes(argPath), UTF_8)
      argText.split(EOL).map(_.trim).filter(_.nonEmpty).toList
    } else List(arg)
  }
}
