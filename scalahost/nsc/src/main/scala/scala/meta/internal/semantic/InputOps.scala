package scala.meta.internal
package semantic

import java.nio.charset.Charset
import scala.collection.mutable
import scala.{meta => m}
import scala.meta.internal.io._
import scala.reflect.internal.util.{Position => GPosition, SourceFile => GSourceFile}
import scala.reflect.io.{PlainFile => GPlainFile}
import scala.util.Try
import org.scalameta.logger

trait InputOps { self: DatabaseOps =>

  private lazy val gSourceFileInputCache = mutable.Map[GSourceFile, m.Input]()
  implicit class XtensionGSourceFileInput(gsource: GSourceFile) {
    def toInput: m.Input =
      gSourceFileInputCache.getOrElseUpdate(gsource, {
        gsource.file match {
          case gfile: GPlainFile =>
            val path = m.AbsolutePath(gfile.file)
            import SemanticdbMode._
            config.semanticdb match {
              case Slim =>
                m.Input.File(path)
              case Fat =>
                val label = path.toRelative(config.sourceroot).toString
                // NOTE: Can't use gsource.content because it's preprocessed by scalac.
                // TODO: Obtain charset from Global.reader.
                val charset = Charset.forName("UTF-8")
                val contents = FileIO.slurp(path, charset)
                m.Input.LabeledString(label, contents)
              case Disabled =>
                m.Input.None
            }
          case other =>
            m.Input.None
        }
      })
  }

  implicit class XtensionGPositionMPosition(pos: GPosition) {
    def toMeta: m.Position = {
      val input = pos.source.toInput
      if (input == m.Input.None) {
        m.Position.None
      } else if (pos.isRange) {
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
