package org.langmeta.internal.io

import java.net.URI
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.util.stream.Collectors
import org.langmeta.io._

object PlatformFileIO {

  def readAllBytes(uri: URI): Array[Byte] = {
    // TODO: URI.toURL isn't yet available in Scala Native,
    // so I had to steal the Scala.js implementation from js/.
    // Please find below the reference implementation from jvm/.
    //
    //   val is = uri.toURL.openStream()
    //   try {
    //     InputStreamIO.readBytes(is)
    //   } finally {
    //     is.close()
    //   }
    if (uri.getScheme == "file") {
      val filepath = Paths.get(uri)
      readAllBytes(AbsolutePath(filepath.toString))
    }
    else throw new UnsupportedOperationException(s"Can't read $uri as InputStream")
  }

  def readAllBytes(path: AbsolutePath): Array[Byte] =
    Files.readAllBytes(path.toNIO)

  def slurp(path: AbsolutePath, charset: Charset): String =
    scala.io.Source.fromFile(path.toFile)(scala.io.Codec(charset)).mkString

  def listFiles(path: AbsolutePath): ListFiles =
    new ListFiles(path, Option(path.toFile.list()).toList.flatten.map(RelativePath.apply))

  def isFile(path: AbsolutePath): Boolean =
    Files.isRegularFile(path.toNIO)

  def isDirectory(path: AbsolutePath): Boolean =
    Files.isDirectory(path.toNIO)

  def listAllFilesRecursively(root: AbsolutePath): ListFiles = {
    // TODO: Some Java stream APIs aren't yet available in Scala Native,
    // so I had to steal the Scala.js implementation from js/.
    // Please find below the reference implementation from jvm/.
    //
    //   import scala.collection.JavaConverters._
    //   val relativeFiles = Files
    //     .walk(root.toNIO)
    //     .collect(Collectors.toList[Path])
    //     .asScala
    //     .collect {
    //       case path if Files.isRegularFile(path) =>
    //         RelativePath(root.toNIO.relativize(path))
    //     }
    //   new ListFiles(root, relativeFiles.toList)
    val builder = List.newBuilder[RelativePath]
    def loop(path: AbsolutePath): Unit = {
      if (path.isDirectory) listFiles(path).foreach(loop)
      else builder += path.toRelative(root)
    }
    loop(root)
    new ListFiles(root, builder.result())
  }
}
