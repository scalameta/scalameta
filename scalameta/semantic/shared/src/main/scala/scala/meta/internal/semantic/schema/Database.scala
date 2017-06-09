package scala.meta
package internal
package semantic
package schema

import scala.collection.immutable.Seq
import scala.meta.inputs.{Input => mInput}
import scala.meta.inputs.{Point => mPoint}
import scala.meta.inputs.{Position => mPosition}
import scala.meta.internal.semantic.{schema => s}
import scala.meta.internal.semantic.{vfs => v}
import scala.meta.io._
import scala.meta.{Dialect => mDialect}
import scala.meta.{semantic => m}
import scala.{Seq => _}

import java.io._

import org.scalameta.data._
import org.scalameta.logger

// NOTE: s.Attributes and friends are generated from semanticdb.proto.
// See scalameta/semantic/jvm/target/scala-<version>/src_managed/main/scala/meta/internal/semantic/schema.
@data
class Database(entries: Seq[Attributes]) {
  def toVfs(targetroot: AbsolutePath): v.Database = {
    val ventries = entries.map { sentry =>
      // TODO: Would it make sense to support multiclasspaths?
      // One use-case for this would be in-place updates of semanticdb files.
      val vpath = v.Paths.scalaToSemanticdb(RelativePath(sentry.filename))
      val fragment = Fragment(targetroot, vpath)
      v.Entry.InMemory(fragment, sentry.toByteArray)
    }
    v.Database(ventries)
  }

  def toMeta(sourcepath: Option[Sourcepath]): m.Database = {
    val mentries = entries.map {
      case s.Attributes(sfilename, scontents, sdialect, snames, smessages, sdenots, ssugars) =>
        assert(sfilename.nonEmpty, "s.Attribute.filename must not be empty")
        val minput = {
          if (scontents == "") {
            val uri =
              sourcepath.getOrElse(sys.error("Sourcepath is required to load slim semanticdb."))
                .find(RelativePath(sfilename))
                .getOrElse(sys.error(s"can't find $sfilename in $sourcepath"))
            mInput.File(AbsolutePath(uri.getPath))
          } else {
            mInput.LabeledString(sfilename.toString, scontents)
          }
        }
        object sRange {
          def unapply(srange: s.Range): Option[mPosition] = {
            val mstart = mPoint.Offset(minput, srange.start)
            val mend = mPoint.Offset(minput, srange.end)
            Some(mPosition.Range(minput, mstart, mend))
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
        object sDenotation {
          def unapply(sdenot: s.Denotation): Option[m.Denotation] = sdenot match {
            case s.Denotation(mflags, mname: String, minfo: String) =>
              Some(m.Denotation(mflags, mname, minfo))
            case _ => None
          }
        }
        val mdialect = {
          val mdialect = Dialect.standards.get(sdialect)
          mdialect.getOrElse(sys.error(s"bad protobuf: unsupported dialect ${sdialect}"))
        }
        val mnames = snames.map {
          case s.ResolvedName(Some(sRange(mpos)), m.Symbol(msym)) => mpos -> msym
          case other => sys.error(s"bad protobuf: unsupported name $other")
        }.toList
        val mmessages = smessages.map {
          case s.Message(Some(sRange(mpos)), sSeverity(msev), mmsg: String) =>
            m.Message(mpos, msev, mmsg)
          case other => sys.error(s"bad protobuf: unsupported message $other")
        }.toList
        val mdenots = sdenots.map {
          case s.SymbolDenotation(m.Symbol(msym), Some(sDenotation(mdenot))) => msym -> mdenot
          case other => sys.error(s"bad protobuf: unsupported denotation $other")
        }.toList
        val msugars = ssugars.map {
          case s.Sugar(Some(sRange(mpos)), msyntax: String) => mpos -> msyntax
          case other => sys.error(s"bad protobuf: unsupported sugar $other")
        }.toList
        minput -> m.Attributes(mdialect, mnames, mmessages, mdenots, msugars)
    }
    m.Database(mentries)
  }
}
