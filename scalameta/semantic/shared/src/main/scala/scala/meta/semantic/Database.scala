package scala.meta
package semantic

import java.io._
import java.net.URI
import java.util.zip._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.util.Try
import org.scalameta.data._
import scala.meta.io._
import scala.meta.inputs._
import scala.meta.inputs.Point.Offset
import scala.meta.inputs.Position.Range
import scala.meta.internal.semantic.codecs._
import scala.meta.internal.semantic.{proto => p}

@data class Database(sources: Seq[AttributedSource]) {
  locally {
    val paths = sources.map(_.path.toString).toSet
    if (sources.size != paths.size) {
      val occs = mutable.Map[String, Int]().withDefault(_ => 0)
      sources.foreach(source => occs(source.path.toString) += 1)
      val duplicates = occs.filter(_._2 > 1).map(_._1)
      sys.error("duplicate paths for " + occs.mkString(", "))
    }
  }

  @deprecated("Use names instead", "1.8.0") def symbols: Map[Position, Symbol] = names
  def names: Map[Position, Symbol] = sources.foldLeft(Map[Position, Symbol]())(_ ++ _.names)
  def messages: Seq[Message] = sources.flatMap(_.messages)
  def denotations: Map[Symbol, Denotation] = sources.foldLeft(Map[Symbol, Denotation]())(_ ++ _.denotations)
  def sugars: Map[Position, String] = sources.foldLeft(Map[Position, String]())(_ ++ _.sugars)

  override def toString: String = sources.mkString(EOL + EOL)
  def toBinary: Array[Byte] = this.toProto[p.Database].toByteArray
  def writeToFile(dir: File): Unit = {
    if (dir.exists) sys.error(s"implementation restriction: can't write a semantic database to an existing file")
    val databaseRoot = Database.locateInClasspath(dir)
    sources.foreach(source => source.writeToFile(new File(source.locateInDatabase(databaseRoot))))
  }
}

object Database {
  private[meta] def locateInClasspath(classpath: File): URI = {
    if (classpath.isDirectory) {
      new File(new File(classpath, "META-INF"), "semanticdb").toURI
    } else if (classpath.getAbsolutePath.endsWith(".jar")) {
      new URI("jar:" + classpath.toURI.toURL + "!META-INF/semanticdb/")
    } else {
      sys.error(s"unsupported classpath entry: $classpath")
    }
  }

  def fromBinary(bytes: Array[Byte]): Database = p.Database.parseFrom(bytes).toMeta[Database]
  def fromFile(file: File): Database = {
    val databaseEntries = mutable.ListBuffer[URI]()
    def addFile(file: File): Unit = {
      if (!file.getPath.endsWith(".semanticdb")) return
      databaseEntries += file.toURI
    }
    def addZipEntry(file: File, entry: ZipEntry): Unit = {
      var relativePath = entry.getName
      if (relativePath.startsWith("/")) relativePath = relativePath.substring(1)
      if (relativePath.endsWith("/")) return
      if (!relativePath.startsWith("META-INF/semanticdb/")) return
      if (!relativePath.endsWith(".semanticdb")) return
      databaseEntries += new URI("jar:" + file.toURI.toURL + "!" + relativePath)
    }
    def explore(file: File): Unit = {
      if (file.isDirectory) {
        val files = file.listFiles
        if (files != null) {
          files.filter(_.isFile).foreach(addFile)
          files.filter(_.isDirectory).foreach(explore)
        }
      } else if (file.getName.endsWith(".jar")) {
        val stream = new FileInputStream(file)
        try {
          val zip = new ZipInputStream(stream)
          var entry = zip.getNextEntry()
          while (entry != null) {
            addZipEntry(file, entry)
            entry = zip.getNextEntry()
          }
        } finally {
          stream.close()
        }
      } else {
        addFile(file)
      }
    }
    val databaseRoot = {
      if (file.isDirectory) new File(new File(file, "META-INF"), "semanticdb")
      else if (file.getAbsolutePath.endsWith(".jar")) file
      else sys.error(s"unsupported file: $file")
    }
    explore(databaseRoot)
    Database(databaseEntries.map(AttributedSource.fromURI).toList)
  }
  def fromClasspath(classpath: String): Database = {
    val fragments = classpath.split(File.pathSeparatorChar).toList.distinct
    Database(fragments.map(s => new File(s)).filter(_.exists).map(fromFile).flatMap(_.sources))
  }
}

@data class AttributedSource(
  path: AbsolutePath,
  names: Map[Position, Symbol],
  messages: Seq[Message],
  denotations: Map[Symbol, Denotation],
  sugars: Map[Position, String]
) {
  override def toString: String = {
    val lines = mutable.ListBuffer[String]()
    def appendSection(name: String, section: List[String]): Unit = {
      if (section.nonEmpty) {
        lines += (name + ":")
        lines ++= section
        lines += ""
      }
    }
    implicit class XtensionPositionRange(pos: Position) {
      def range = s"[${pos.start.offset}..${pos.end.offset})"
    }

    lines.append(path.toString, "-" * path.toString.length, "")

    val s_names = names.toList.sortBy(_._1.start.offset).map {
      case ((pos, symbol)) =>
        s"${pos.range}: ${pos.text} => $symbol"
    }
    appendSection("Names", s_names)

    val s_messages = messages.toList.sortBy(_.position.start.offset).map {
      case Message(pos, severity, message) =>
        s"${pos.range}: [${severity.toString.toLowerCase}] ${message}"
    }
    appendSection("Messages", s_messages)

    val s_denotations = denotations.toList.sortBy(_._1.syntax).map {
      case ((symbol, denotation)) =>
        s"$symbol => $denotation"
    }
    appendSection("Denotations", s_denotations)

    val s_sugars = sugars.toList.sortBy(_._1.start.offset).map {
      case ((pos, syntax)) =>
        s"${pos.range} $syntax"
    }
    appendSection("Sugars", s_sugars)

    lines.mkString(EOL)
  }

  def toBinary: Array[Byte] = this.toProto[p.AttributedSource].toByteArray

  def writeToFile(file: File): Unit = {
    file.getParentFile.mkdirs()
    val fos = new FileOutputStream(file)
    try fos.write(this.toProto[p.AttributedSource].toByteArray)
    finally { fos.close() }
  }

  private[meta] def locateInDatabase(databaseRoot: URI): URI = {
    var result = databaseRoot.toString
    if (!result.endsWith("/")) result += "/"
    // TODO: This relativization is unsound and should be removed.
    result += path.toRelative.toString
    result = result.stripSuffix(".scala") + ".semanticdb"
    new URI(result)
  }
}

object AttributedSource {
  def fromBinary(bytes: Array[Byte]): AttributedSource = {
    p.AttributedSource.parseFrom(bytes).toMeta[AttributedSource]
  }

  def fromFile(file: File): AttributedSource = {
    p.AttributedSource.parseFrom(new FileInputStream(file)).toMeta[AttributedSource]
  }

  def fromURI(uri: URI): AttributedSource = {
    p.AttributedSource.parseFrom(uri.toURL.openStream).toMeta[AttributedSource]
  }
}