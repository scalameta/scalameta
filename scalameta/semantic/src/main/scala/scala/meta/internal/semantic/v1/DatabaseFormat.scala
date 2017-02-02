package scala.meta
package internal
package semantic
package v1

import java.io._
import java.net.URI
import java.nio.charset.Charset
import scala.io._
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.meta.semantic.v1.Database
import scala.meta.semantic.v1.Location
import scala.meta.semantic.v1.Symbol

// TODO: This is obviously a very naive implementation, optimized for easy eyeballing.
// In the future, I'd like to have something that takes less space and is more performant.

trait DatabaseFormat {
  def toBinary(database: Database): Array[Byte] = {
    val databaseBuf = new StringBuilder
    val grouped = database.symbols.groupBy(_._1.uri)
    grouped.keys.toList.sorted.foreach(uri => {
      databaseBuf ++= (uri.stripPrefix("file:") + EOL)
      val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
      val content = scala.io.Source.fromFile(new java.io.File(uri.stripPrefix("file:")))(codec).mkString
      grouped(uri).keys.toList.sortBy(_.start).foreach(k => {
        val snippet = content.substring(k.start, k.end)
        databaseBuf ++= (s"[${k.start}..${k.end}): $snippet => ${grouped(uri)(k).id}" + EOL)
      })
      databaseBuf ++= EOL
    })
    val databasePayload = databaseBuf.toString
    databasePayload.getBytes(Charset.forName("UTF-8"));
  }

  def writeFile(file: File, database: Database): Unit = {
    val fos = new FileOutputStream(file)
    try fos.write(Database.toBinary(database))
    finally { fos.close() }
  }

  def fromBinary(bytes: Array[Byte]): Database = {
    val symbols = mutable.Map[Location, Symbol]()
    val lines = new String(bytes, Charset.forName("UTF-8")).split(EOL)
    var i = 0

    def readSection(): Map[Location, Symbol] = {
      val section = mutable.Map[Location, Symbol]()
      val header = lines(i)
      i += 1

      while (i < lines.length && lines(i) != "") {
        // [7..11): Test => _empty_.Test.
        val Mapping = """^\[(\d+)\.\.(\d+)\): .*? => (.*)$""".r
        val line = lines(i)
        i += 1
        try {
          line match {
            case Mapping(s_start, s_end, id) =>
              val location = Location(new URI("file:" + header).toString, s_start.toInt, s_end.toInt)
              val symbol = Symbol(id)
              section(location) = symbol
            case _ =>
              sys.error("unexpected line format")
          }
        } catch {
          case ex: Exception =>
            sys.error(s"$i:${ex.getMessage}$EOL$line")
        }
      }

      section.toMap
    }

    while (i < lines.length) {
      val section = readSection()
      symbols.retain((k, _) => k.uri != section.head._1.uri)
      symbols ++= section
    }

    Database(symbols.toMap)
  }

  def readFile(file: File): Database = {
    val fis = new FileInputStream(file)
    try {
      val length = fis.available
      val bytes = new Array[Byte](length)
      fis.read(bytes)
      Database.fromBinary(bytes)
    } finally {
      fis.close()
    }
  }
}