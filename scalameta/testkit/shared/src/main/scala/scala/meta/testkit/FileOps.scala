package scala.meta.testkit

import java.io.File
import java.nio.file.Files
import java.nio.file.Paths

object FileOps {
  def workingDirectory: File = new File(".")
  def getFile(first: String, str: String*): File = Paths.get(first, str: _*).toFile
  def readFile(file: File): String = new String(Files.readAllBytes(Paths.get(file.toURI)))

  def listFiles(file: File): Iterator[String] =
    if (file.isFile) Iterator.single(file.getAbsolutePath)
    else {
      def listFilesIter(s: File): Iterator[String] = Option(s.listFiles())
        .fold[Iterator[String]](Iterator.empty) { all =>
          val dirs = List.newBuilder[File]
          val files = List.newBuilder[String]
          all.foreach(x => if (x.isFile) files += x.getPath else dirs += x)
          files.result().iterator ++ dirs.result().iterator.flatMap(listFilesIter)
        }
      listFilesIter(file)
    }
}
