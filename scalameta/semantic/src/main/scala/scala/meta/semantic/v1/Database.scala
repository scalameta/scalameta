package scala.meta
package semantic
package v1

import scala.meta.prettyprinters._
import java.io._
import java.nio.charset.Charset
import scala.io._
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.meta.semantic.v1._
import scala.meta.internal.semantic.v1._

// NOTE: This is an initial take on the semantic API.
// Instead of immediately implementing the full vision described in my dissertation,
// we will first deliver the low-hanging fruit (https://github.com/scalameta/scalameta/issues/604),
// and only then will approach really tricky tasks (https://github.com/scalameta/scalameta/issues/623).

case class Database(
  symbols: Map[Location, Symbol]
  // TODO: Additional fields are to be discussed
  // https://github.com/scalameta/scalameta/issues/605
) {
  override def toString: String = {
    val buf = new StringBuilder
    val grouped = symbols.groupBy(_._1.addr)
    grouped.keys.toList.sortBy(_.syntax).foreach(addr => {
      buf ++= (addr + EOL)
      val content = addr.content
      grouped(addr).keys.toList.sortBy(_.start).foreach(k => {
        val snippet = content.substring(k.start, k.end)
        buf ++= (s"[${k.start}..${k.end}): $snippet => ${grouped(addr)(k).syntax}" + EOL)
      })
      buf ++= EOL
    })
    buf.toString.trim + EOL
  }

  def toBinary: Array[Byte] = {
    // TODO: This is obviously a very naive implementation, optimized for easy eyeballing.
    // In the future, I'd like to have something that takes less space and is more performant.
    toString.getBytes(Charset.forName("UTF-8"));
  }

  def toFile(file: File): Unit = {
    val fos = new FileOutputStream(file)
    try fos.write(toBinary)
    finally { fos.close() }
  }
}

object Database extends DatabaseOps {
  def apply(): Database = {
    Database(Map[Location, Symbol]())
  }

  def apply(bytes: Array[Byte]): Database = {
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
            case Mapping(s_start, s_end, s_id) =>
              val location = Location(Address(header), s_start.toInt, s_end.toInt)
              val symbol = Symbol(s_id)
              section(location) = symbol
            case _ =>
              sys.error("unexpected line format")
          }
        } catch {
          case ex: Exception =>
            sys.error(s"$i:${ex.getMessage}$EOL$line")
        }
      }
      i += 1

      section.toMap
    }

    while (i < lines.length) {
      val section = readSection()
      symbols.retain((k, _) => k.addr != section.head._1.addr)
      symbols ++= section
    }

    Database(symbols.toMap)
  }

  def apply(file: File): Database = {
    val fis = new FileInputStream(file)
    try {
      val length = fis.available
      val bytes = new Array[Byte](length)
      fis.read(bytes)
      Database(bytes)
    } finally {
      fis.close()
    }
  }
}