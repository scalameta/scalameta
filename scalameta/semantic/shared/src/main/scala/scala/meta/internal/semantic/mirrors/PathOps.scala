package scala.meta
package internal
package semantic
package mirrors

import java.io._
import java.net.URI
import java.util.zip._
import scala.collection.mutable

trait PathOps {
  implicit class XtensionMultipath(s: String) {
    def paths: List[URI] = {
      s.split(File.pathSeparatorChar).map(s => new File(s).toURI).toList
    }

    def files: List[File] = {
      val result = mutable.ListBuffer[URI]()
      s.paths.foreach(s => {
        def addFile(file: File): Unit = {
          result += file.toURI
        }
        def addZipEntry(file: File, entry: ZipEntry): Unit = {
          var relativePath = entry.getName
          if (relativePath.startsWith("/")) relativePath = relativePath.substring(1)
          if (relativePath.endsWith("/")) return
          result += new URI("jar:" + file.toURI.toURL + "!" + entry.getName)
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
        explore(new File(s))
      })
      result.map(new File(_)).filter(_.isFile).toList
    }
  }
}
