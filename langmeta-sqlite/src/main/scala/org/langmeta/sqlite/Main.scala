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
        val docsToPaths = mutable.Map[String, Path]()
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
            val columnValues = "?" * columnCount
            val s_values = "(" + columnValues.mkString(", ") + ")"
            val sql = s"insert into $table $s_columns values $s_values"
            conn.prepareStatement(sql)
          }
          val namesStmt = tableInsertStmt("names")

          val semanticdbStart = System.nanoTime()
          var nameCount = 0
          var messageCount = 0
          var symbolCount = 0
          val syntheticCount = 0
          semanticdbFilenames.foreach { semanticdbFilename =>
            val path = Paths.get(semanticdbFilename)
            val bytes = Files.readAllBytes(path)
            val db = Database.parseFrom(bytes)
            db.documents.foreach { doc =>
              docsToPaths.get(doc.filename) match {
                case Some(existingPath) =>
                  val what = doc.filename
                  val details = "both $existingPath and $path"
                  println(s"Duplicate document filename $what in $details")
                case None =>
                  docsToPaths(doc.filename) = path
                  doc.names.foreach { name =>
                    nameCount += 1
                    namesStmt.setString(1, doc.filename)
                    namesStmt.setInt(2, name.position.get.start)
                    namesStmt.setInt(3, name.position.get.end)
                    namesStmt.setString(4, name.symbol)
                    namesStmt.setBoolean(5, name.isDefinition)
                    namesStmt.executeUpdate()
                  }
                  if ((docsToPaths.size % 1000) == 0) {
                    val semanticdbElapsed = System.nanoTime() - semanticdbStart
                    val buf = new StringBuilder
                    buf.append(s"${docsToPaths.size} docs: ")
                    buf.append(s"$nameCount names, ")
                    buf.append(s"$messageCount messages, ")
                    buf.append(s"$symbolCount symbols, ")
                    buf.append(s"$syntheticCount synthetics")
                    buf.append(s" (~${semanticdbElapsed / 1000000000}s) ")
                    println(buf.toString)
                  }
              }
            }
          }
        } finally {
          conn.commit()
          conn.close()
          val appElapsed = (System.nanoTime() - appStart) * 1.0 / 1000000000
          val appPerformance = docsToPaths.size / appElapsed
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