package scala.meta.internal
package semantic

import java.nio.charset.Charset
import scala.collection.mutable
import scala.{meta => m}
import scala.meta.internal.io._
import scala.reflect.internal.util.{Position => GPosition, SourceFile => GSourceFile}
import scala.reflect.io.{PlainFile => GPlainFile}

trait InputOps { self: DatabaseOps =>

  private lazy val gSourceFileInputCache = mutable.Map[GSourceFile, m.Input]()
  implicit class XtensionGSourceFileInput(gsource: GSourceFile) {
    def toInput: m.Input =
      gSourceFileInputCache.getOrElseUpdate(gsource, {
        val path = gsource.file match {
          case gfile: GPlainFile => m.AbsolutePath(gfile.file)
          case other => sys.error(s"unsupported file " + other)
        }
        if (config.semanticdb.isSlim) {
          m.Input.File(path)
        } else if (config.semanticdb.isFat) {
          val labelOpt = config.sourcepath.relativize(path.toURI).map(_.toString)
          val label = labelOpt.getOrElse(sys.error(s"can't find $path in ${config.sourcepath}"))
          // NOTE: Can't use gsource.content because it's preprocessed by scalac.
          // TODO: Obtain charset from Global.reader.
          val charset = Charset.forName("UTF-8")
          val contents = FileIO.slurp(path, charset)
          m.Input.LabeledString(label, contents)
        } else {
          sys.error(s"unsupported configuration $config")
        }
      })
  }

  implicit class XtensionGPositionMPosition(pos: GPosition) {
    def toMeta: m.Position = {
      val input = pos.source.toInput
      if (pos.isRange) {
        m.Position.Range(input, m.Point.Offset(input, pos.start), m.Point.Offset(input, pos.end))
      } else {
        // NOTE: Even with -Yrangepos enabled we cannot be guaranteed that all positions are
        // range positions. In the case we encounter a non-range position we assume start == end.
        val mpoint = m.Point.Offset(input, pos.point)
        m.Position.Range(input, mpoint, mpoint)
      }
    }
  }
}
