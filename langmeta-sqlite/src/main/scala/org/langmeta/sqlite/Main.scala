package org.langmeta.sqlite

import java.io._
import java.nio.file._
import java.sql._
import scala.Array
import scala.collection.mutable
import scala.io.Codec
import scala.reflect.classTag
import scala.util.control.NonFatal
import org.langmeta.internal.semanticdb.schema._

object Main {
  def main(args: Array[String]): Unit = {
    args match {
      case Array(sqliteFilename, semanticdbFilenames @ _*) =>
        val appStart = System.nanoTime()
        var genuineDocuments = 0
        class Counter { var value = 0; def next() = { value += 1; value }; }
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
          val messageStmt = tableInsertStmt("message")
          val symbolStmt = tableInsertStmt("symbol")
          val syntheticStmt = tableInsertStmt("synthetic")

          val semanticdbStart = System.nanoTime()
          val documentsToPaths = mutable.Map[String, Path]()
          var documentId = new Counter()
          var nameId = new Counter()
          var messageId = new Counter()
          var _symbolId = new Counter()
          var symbolIds = mutable.Map[String, Int]()
          var symbolTodo = mutable.Map[Int, String]()
          var symbolDone = mutable.Set[Int]()
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
            try {
              val bytes = Files.readAllBytes(path)
              val db = Database.parseFrom(bytes)
              db.documents.foreach { document =>
                documentsToPaths.get(document.filename) match {
                  case Some(existingPath) =>
                    val what = document.filename
                    val details = "both $existingPath and $path"
                    println(s"Duplicate document filename $what in $details")
                  case None =>
                    genuineDocuments += 1
                    documentsToPaths(document.filename) = path

                    val documentRef = documentId.next
                    documentStmt.setInt(1, documentRef)
                    documentStmt.setString(2, document.filename)
                    documentStmt.setString(3, document.contents)
                    documentStmt.setString(4, document.language)
                    documentStmt.executeUpdate()

                    document.names.foreach { name =>
                      nameStmt.setInt(1, nameId.next)
                      nameStmt.setInt(2, documentRef)
                      nameStmt.setInt(3, name.position.get.start)
                      nameStmt.setInt(4, name.position.get.end)
                      nameStmt.setInt(5, symbolId(name.symbol))
                      nameStmt.setBoolean(6, name.isDefinition)
                      nameStmt.executeUpdate()
                    }

                    document.messages.foreach { message =>
                      messageStmt.setInt(1, messageId.next)
                      messageStmt.setInt(2, documentRef)
                      messageStmt.setInt(3, message.position.get.start)
                      messageStmt.setInt(4, message.position.get.end)
                      messageStmt.setInt(5, message.severity.value)
                      messageStmt.setString(6, message.text)
                      messageStmt.executeUpdate()
                    }

                    document.symbols.foreach { symbol =>
                      val symbolRef = symbolId(symbol.symbol)
                      if (!symbolDone.contains(symbolRef)) {
                        symbolStmt.setInt(1, symbolRef)
                        symbolStmt.setString(2, symbol.symbol)
                        symbolStmt.setLong(3, symbol.denotation.get.flags)
                        symbolStmt.setString(4, symbol.denotation.get.name)
                        val signatureDocumentRef = {
                          documentStmt.setInt(1, documentId.next)
                          documentStmt.setString(2, null)
                          documentStmt.setString(3, symbol.denotation.get.signature)
                          documentStmt.setString(4, null)
                          documentStmt.executeUpdate()
                          documentId.value
                        }
                        symbolStmt.setInt(5, signatureDocumentRef)
                        symbol.denotation.get.names.foreach { name =>
                          nameStmt.setInt(1, nameId.next)
                          nameStmt.setInt(2, signatureDocumentRef)
                          nameStmt.setInt(3, name.position.get.start)
                          nameStmt.setInt(4, name.position.get.end)
                          nameStmt.setInt(5, symbolId(name.symbol))
                          nameStmt.setBoolean(6, name.isDefinition)
                          nameStmt.executeUpdate()
                        }
                        symbolStmt.executeUpdate()
                        symbolTodo.remove(symbolRef)
                        symbolDone.add(symbolRef)
                      }
                    }

                    document.synthetics.foreach { synthetic =>
                      syntheticStmt.setInt(1, syntheticId.next)
                      syntheticStmt.setInt(2, synthetic.pos.get.start)
                      syntheticStmt.setInt(3, synthetic.pos.get.end)
                      val syntheticDocumentRef = {
                        documentStmt.setInt(1, documentId.next)
                        documentStmt.setString(2, null)
                        documentStmt.setString(3, synthetic.text)
                        documentStmt.setString(4, null)
                        documentStmt.executeUpdate()
                        documentId.value
                      }
                      syntheticStmt.setInt(4, syntheticDocumentRef)
                      synthetic.names.foreach { name =>
                        nameStmt.setInt(1, nameId.next)
                        nameStmt.setInt(2, syntheticDocumentRef)
                        nameStmt.setInt(3, name.position.get.start)
                        nameStmt.setInt(4, name.position.get.end)
                        nameStmt.setInt(5, symbolId(name.symbol))
                        nameStmt.setBoolean(6, name.isDefinition)
                        nameStmt.executeUpdate()
                      }
                      syntheticStmt.executeUpdate()
                    }

                    if ((genuineDocuments % 1000) == 0) {
                      val semanticdbElapsed = System.nanoTime() - semanticdbStart
                      val buf = new StringBuilder
                      buf.append(s"$genuineDocuments documents: ")
                      buf.append(s"${nameId.value} names, ")
                      buf.append(s"${messageId.value} messages, ")
                      buf.append(s"${_symbolId.value} symbols, ")
                      buf.append(s"${syntheticId.value} synthetics")
                      buf.append(s" (~${semanticdbElapsed / 1000000000}s) ")
                      println(buf.toString)
                    }
                }
              }
            } catch {
              case NonFatal(ex) =>
                println(s"Error processing $path")
                ex.printStackTrace
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
          val appCPU = (System.nanoTime() - appStart) * 1.0 / 1000000000
          val appDisk = new File(sqliteFilename).length * 1.0 / 1024 / 1024
          val appPerformance = genuineDocuments / appCPU
          println(s"CPU time: ${"%.3f".format(appCPU)}s")
          println(s"Disk size: ${"%.1f".format(appDisk)} MB")
          println(s"Performance: ${"%.3f".format(appPerformance)} documents/s")
        }
      case _ =>
        val objectName = classTag[Main.type].toString.stripSuffix("$")
        println(s"usage: $objectName </path/to/sqlite.db> [<glob> <glob> ...]")
        sys.exit(1)
    }
  }
}