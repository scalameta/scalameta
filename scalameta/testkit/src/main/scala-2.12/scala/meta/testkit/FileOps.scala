package scala.meta.testkit

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

import geny.Generator

object FileOps {
  def workingDirectory: File = new File(".")
  def getFile(first: String, str: String*): File =
    Paths.get(first, str: _*).toFile
  def readFile(file: File): String =
    new String(Files.readAllBytes(Paths.get(file.toURI)))

  def listFiles(file: File): Generator[String] = {
    if (file.isFile) {
      Generator(file.getAbsolutePath)
    } else {
      def listFilesIter(s: File): Iterable[String] = {
        val (dirs, files) =
          Option(s.listFiles()).toIterable
            .flatMap(_.toIterator)
            .partition(_.isDirectory)
        files.map(_.getPath) ++ dirs.flatMap(listFilesIter)
      }
      for {
        f0 <- Option(listFilesIter(file)).toVector
        filename <- f0
      } yield filename
    }
  }
}
