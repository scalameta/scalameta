package org.langmeta.sqlite

import java.nio.file._
import java.sql._
import scala.Array
import scala.collection.mutable
import scala.io.Codec
import scala.reflect.classTag
import org.langmeta.internal.semanticdb.schema._

object Main {
  def main(args: Array[String]): Unit = {
    args match {
      case Array(sqliteFilename, semanticdbFilenames @ _*) =>
        val appStart = System.nanoTime()
        class Counter { var value = 0; def next() = { value += 1; value }; }
        var documentId = new Counter()
        val sqlitePath = Paths.get(sqliteFilename)
        if (Files.exists(sqlitePath)) sys.error(s"$sqlitePath already exists")
        val conn = DriverManager.getConnection(s"jdbc:sqlite:$sqliteFilename")
        conn.setAutoCommit(false)
        try {
          val ddlUrl = getClass.getClassLoader.getResource("semanticdb.ddl")
          val ddlStream = ddlUrl.openStream
          val ddl = scala.io.Source.fromInputStream(ddlStream)(Codec.UTF8)
          val ddlStmt = conn.createStatement()
          ddlStmt.executeUpdate(ddl.mkString)

          def tableInsertStmt(table: String): PreparedStatement = {
            val rs = ddlStmt.executeQuery("select * from " + table)
            val rsmd = rs.getMetaData()
            val columnCount = rsmd.getColumnCount()
            val columnNames = 1.to(columnCount).map(rsmd.getColumnName)
            val s_columns = "(" + columnNames.mkString(", ") + ")"
            val columnValues = "?" * columnNames.length
            val s_values = "(" + columnValues.mkString(", ") + ")"
            val sql = s"insert into $table $s_columns values $s_values"
            conn.prepareStatement(sql)
          }
          val documentStmt = tableInsertStmt("document")
          val nameStmt = tableInsertStmt("name")
          val symbolStmt = tableInsertStmt("symbol")

          val semanticdbStart = System.nanoTime()
          val documentsToPaths = mutable.Map[String, Path]()
          var nameId = new Counter()
          var messageId = new Counter()
          var _symbolId = new Counter()
          var symbolIds = mutable.Map[String, Int]()
          var symbolTodo = mutable.Map[Int, String]()
          def symbolId(symbol: String): Int = {
            if (symbolIds.contains(symbol)) {
              symbolIds(symbol)
            } else {
              val symbolId = _symbolId.next()
              symbolIds(symbol) = symbolId
              symbolTodo(symbolId) = symbol
              symbolId
            }
          }
          val syntheticId = new Counter()

          semanticdbFilenames.foreach { semanticdbFilename =>
            val path = Paths.get(semanticdbFilename)
            val bytes = Files.readAllBytes(path)
            val db = Database.parseFrom(bytes)
            db.documents.foreach { document =>
              documentsToPaths.get(document.filename) match {
                case Some(existingPath) =>
                  val what = document.filename
                  val details = "both $existingPath and $path"
                  println(s"Duplicate document filename $what in $details")
                case None =>
                  documentsToPaths(document.filename) = path
                  documentStmt.setInt(1, documentId.next)
                  documentStmt.setString(2, document.filename)
                  documentStmt.setString(3, document.contents)
                  documentStmt.setString(4, document.language)
                  documentStmt.executeUpdate()

                  document.names.foreach { name =>
                    nameStmt.setInt(1, nameId.next)
                    nameStmt.setInt(2, documentId.value)
                    nameStmt.setInt(3, name.position.get.start)
                    nameStmt.setInt(4, name.position.get.end)
                    nameStmt.setInt(5, symbolId(name.symbol))
                    nameStmt.setBoolean(6, name.isDefinition)
                    nameStmt.executeUpdate()
                  }

                  if ((documentId.value % 1000) == 0) {
                    val semanticdbElapsed = System.nanoTime() - semanticdbStart
                    val buf = new StringBuilder
                    buf.append(s"${documentId.value} documents: ")
                    buf.append(s"${nameId.value} names, ")
                    buf.append(s"${messageId.value} messages, ")
                    buf.append(s"${_symbolId.value} symbols, ")
                    buf.append(s"${syntheticId.value} synthetics")
                    buf.append(s" (~${semanticdbElapsed / 1000000000}s) ")
                    println(buf.toString)
                  }
              }
            }
          }

          // NOTE: Remaining symbols that haven't yet been mentioned
          // in document.symbols must be flushed using default metadata.
          symbolTodo.foreach {
            case (symbolId, symbol) =>
              symbolStmt.setInt(1, symbolId)
              symbolStmt.setString(2, symbol)
              symbolStmt.setInt(3, 0)
              symbolStmt.setString(4, null)
              symbolStmt.setInt(5, 0)
              symbolStmt.executeUpdate()
          }
        } finally {
          conn.commit()
          conn.close()
          val appElapsed = (System.nanoTime() - appStart) * 1.0 / 1000000000
          val appPerformance = documentId.value / appElapsed
          println(s"Elapsed: ${"%.3f".format(appElapsed)}s")
          println(s"Performance: ${"%.3f".format(appPerformance)} docs/s")
        }
      case _ =>
        val objectName = classTag[Main.type].toString.stripSuffix("$")
        println(s"usage: $objectName </path/to/sqlite.db> [<glob> <glob> ...]")
        sys.exit(1)
    }
  }
}