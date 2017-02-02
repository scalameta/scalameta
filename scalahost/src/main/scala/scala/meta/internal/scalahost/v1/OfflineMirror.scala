package scala.meta.internal
package scalahost
package v1

import java.io._
import java.net.URI
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.semantic.v1.Database
import scala.meta.semantic.v1.Location

class OfflineMirror(classpath: String, sourcepath: String)
    extends Mirror
    with DialectOps
    with MirrorOps
    with PathOps {
  if (classpath == "") sys.error("Classpath must be non-empty")
  if (sourcepath == "") sys.error("Sourcepath must be non-empty")

  override def toString: String = {
    s"online mirror for $classpath and $sourcepath"
  }

  lazy val sources: Seq[Source] = {
    sourcepath.files.map(uri => dialect(new File(uri)).parse[Source].get)
  }

  lazy val database: Database = {
    val symbols = mutable.Map[Location, Symbol]()

    val databaseFiles = classpath.paths
      .flatMap(uri => {
        val subfiles = new File(uri).listFiles
        if (subfiles != null) subfiles.filter(_.getName.endsWith(".semanticdb")).toList
        else Nil
      })
      .sortBy(_.getName)
    databaseFiles.foreach(databaseFile => {
      val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
      val lines = scala.io.Source.fromFile(databaseFile)(codec).getLines.toList
      var i     = 0

      def readSection(): Map[Location, Symbol] = {
        val section = mutable.Map[Location, Symbol]()
        val header  = lines(i)
        i += 1

        while (i < lines.length && lines(i) != "") {
          // [7..11): Test => _empty_.Test.
          val Mapping = """^\[(\d+)\.\.(\d+)\): .*? => (.*)$""".r
          val line    = lines(i)
          i += 1
          try {
            line match {
              case Mapping(s_start, s_end, id) =>
                val location =
                  Location(new URI("file:" + header).toString, s_start.toInt, s_end.toInt)
                val symbol = Symbol(id)
                section(location) = symbol
              case _ =>
                sys.error("unexpected line format")
            }
          } catch {
            case ex: Exception =>
              val header = s"${databaseFile.getAbsolutePath}:$i:${ex.getMessage}"
              sys.error(s"$header$EOL$line")
          }
        }

        section.toMap
      }

      while (i < lines.length) {
        val section = readSection()
        symbols.retain((k, _) => k.uri != section.head._1.uri)
        symbols ++= section
      }
    })

    Database(symbols.toMap)
  }
}
