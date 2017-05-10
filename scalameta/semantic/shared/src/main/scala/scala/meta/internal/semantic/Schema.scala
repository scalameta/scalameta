package scala.meta
package internal
package semantic

import java.net.URI
import org.scalameta.data._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.io._
import scala.meta.internal.semantic.{vfs => v}
import scala.meta.internal.semantic.{schema => s}
import scala.meta.{semantic => m}
import scala.meta.inputs.{Input => mInput, Position => mPosition, Point => mPoint}

package object schema {
  @data class Database(entries: Seq[Attributes]) {
    def toVfs(classpath: Classpath, sourcepath: Sourcepath): v.Database = {
      if (classpath.shallow.isEmpty) sys.error("can't save semanticdb to an empty classpath")
      val ventries = entries.map(entry => {
        // TODO: Would it make sense to support multiclasspaths?
        // One use-case for this would be in-place updates of semanticdb files.
        val base = AbsolutePath(classpath.shallow.head)
        val scalaNameOpt = sourcepath.relativize(AbsolutePath(entry.path).toURI)
        val scalaName = scalaNameOpt.getOrElse(sys.error(s"can't find ${entry.path} in $sourcepath"))
        val semanticdbName = v.Paths.scalaToSemanticdb(scalaName)
        val fragment = Fragment(base, semanticdbName)
        v.Entry.InMemory(fragment, entry.toByteArray)
      })
      v.Database(ventries)
    }

    def toMeta: m.Database = {
      val mentries = entries.map {
        case s.Attributes(spath, sdialect, snames, smessages, sdenots, ssugars) =>
          val minput = mInput.File(AbsolutePath(spath))
          object sRange {
            def unapply(srange: s.Range): Option[mPosition] = {
              val mstart = mPoint.Offset(minput, srange.start)
              val mend = mPoint.Offset(minput, srange.end)
              if (srange.point > srange.start && srange.point < srange.end) {
                val mpoint = mPoint.Offset(minput, srange.point)
                Some(mPosition.RangeWithPoint(minput, mstart, mpoint, mend))
              }
              else Some(mPosition.Range(minput, mstart, mend))
            }
          }
          object sSymbol {
            def unapply(ssym: String): Option[m.Symbol] = {
              scala.util.Try(m.Symbol(ssym)).toOption
            }
          }
          object sSeverity {
            def unapply(ssev: s.Message.Severity): Option[m.Severity] = {
              ssev match {
                case s.Message.Severity.INFO => Some(m.Severity.Info)
                case s.Message.Severity.WARNING => Some(m.Severity.Warning)
                case s.Message.Severity.ERROR => Some(m.Severity.Error)
                case _ => None
              }
            }
          }
          object sString {
            def unapply(s: String): Option[String] = {
              if (s != null) Some(s) else None
            }
          }
          object sDenotation {
            def unapply(sdenot: s.Denotation): Option[m.Denotation] = sdenot match {
              case s.Denotation(mflags, sString(mname), sString(minfo)) => Some(m.Denotation(mflags, mname, minfo))
              case _ => None
            }
          }
          val mdialect = {
            val mdialect = Dialect.standards.get(sdialect)
            mdialect.getOrElse(sys.error(s"bad protobuf: unsupported dialect ${sdialect}"))
          }
          val mnames = snames.map {
            case s.ResolvedName(Some(sRange(mpos)), sSymbol(msym)) => mpos -> msym
            case other => sys.error(s"bad protobuf: unsupported name $other")
          }.toList
          val mmessages = smessages.map {
            case s.Message(Some(sRange(mpos)), sSeverity(msev), sString(mmsg)) => m.Message(mpos, msev, mmsg)
            case other => sys.error(s"bad protobuf: unsupported message $other")
          }.toList
          val mdenots = sdenots.map {
            case s.SymbolDenotation(sSymbol(msym), Some(sDenotation(mdenot))) => msym -> mdenot
            case other => sys.error(s"bad protobuf: unsupported denotation $other")
          }.toList
          val msugars = ssugars.map {
            case s.Sugar(Some(sRange(mpos)), sString(msyntax)) => mpos -> msyntax
            case other => sys.error(s"bad protobuf: unsupported sugar $other")
          }.toList
          minput -> m.Attributes(mdialect, mnames, mmessages, mdenots, msugars)
      }
      m.Database(mentries)
    }
  }

  // NOTE: s.Attributes and friends are generated from semanticdb.proto.
  // See scalameta/semantic/jvm/target/scala-<version>/src_managed/main/scala/meta/internal/semantic/schema.
}