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
import scala.util.Try
import scala.{Seq => _}

import java.io._
import java.nio.charset.Charset

import org.scalameta.data._

// NOTE: s.Attributes and friends are generated from semanticdb.proto.
// See scalameta/semantic/jvm/target/scala-<version>/src_managed/main/scala/meta/internal/semantic/schema.
@data class Database(entries: Seq[(RelativePath, Attributes)]) {
  def toVfs(classpath: Classpath): v.Database = {
    if (classpath.shallow.isEmpty) sys.error("can't save semanticdb to an empty classpath")
    val ventries = entries.map {
      case (spath, sentry) =>
        // TODO: Would it make sense to support multiclasspaths?
        // One use-case for this would be in-place updates of semanticdb files.
        val base = AbsolutePath(classpath.shallow.head)
        val vpath = v.Paths.scalaToSemanticdb(spath)
        val fragment = Fragment(base, vpath)
        v.Entry.InMemory(fragment, sentry.toByteArray)
    }
    v.Database(ventries)
  }

  def toMeta(sourcepath: Sourcepath): m.Database = {
    val mentries = entries.map {
      case (spath, s.Attributes(scontents, sdialect, snames, smessages, sdenots, ssugars)) =>
        val minput = {
          if (scontents == "") {
            val uri =
              sourcepath.find(spath).getOrElse(sys.error(s"can't find $spath in $sourcepath"))
            mInput.File(new File(uri))
          } else {
            mInput.LabeledString(spath.toString, scontents)
          }
        }
        object sRange {
          def unapply(srange: s.Range): Option[mPosition] = {
            val mstart = mPoint.Offset(minput, srange.start)
            val mend = mPoint.Offset(minput, srange.end)
            Some(mPosition.Range(minput, mstart, mend))
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
            case s.Denotation(mflags, sString(mname), sString(minfo)) =>
              Some(m.Denotation(mflags, mname, minfo))
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
          case s.Message(Some(sRange(mpos)), sSeverity(msev), sString(mmsg)) =>
            m.Message(mpos, msev, mmsg)
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

object Database {
  def fromMeta(mdatabase: m.Database, sourcepath: Sourcepath): s.Database = {
    val sentries = mdatabase.entries.map {
      case (minput, m.Attributes(mdialect, mnames, mmessages, mdenots, msugars)) =>
        object mRange {
          def unapply(mpos: mPosition): Option[s.Range] = mpos match {
            case mPosition.Range(`minput`, mPoint.Offset(_, sstart), mPoint.Offset(_, send)) =>
              Some(s.Range(sstart, send))
            case _ =>
              None
          }
        }
        object mSymbol {
          def unapply(msym: m.Symbol): Option[String] = Some(msym.syntax)
        }
        object mSeverity {
          def unapply(msev: m.Severity): Option[s.Message.Severity] = {
            msev match {
              case m.Severity.Info => Some(s.Message.Severity.INFO)
              case m.Severity.Warning => Some(s.Message.Severity.WARNING)
              case m.Severity.Error => Some(s.Message.Severity.ERROR)
              case _ => None
            }
          }
        }
        object mDenotation {
          def unapply(mdenot: m.Denotation): Option[s.Denotation] = mdenot match {
            case m.Denotation(sflags, sname, sinfo) => Some(s.Denotation(sflags, sname, sinfo))
            case _ => None
          }
        }
        val (spath, scontents) = minput match {
          case mInput.File(path, charset) if charset == Charset.forName("UTF-8") =>
            val spath = sourcepath.relativize(path.toURI)
            spath.getOrElse(sys.error(s"bad database: can't find $path in $sourcepath")) -> ""
          case mInput.LabeledString(label, contents) =>
            val spathOpt = Try(RelativePath(label))
            val spath = spathOpt.getOrElse(sys.error(s"bad database: unsupported label $label"))
            spath -> contents
          case other =>
            sys.error(s"bad database: unsupported input $other")
        }
        val sdialect = {
          val sdialect = mDialect.standards.find(_._2 == mdialect).map(_._1)
          sdialect.getOrElse(sys.error(s"bad database: unsupported dialect $mdialect"))
        }
        val snames = mnames.map {
          case (mRange(srange), mSymbol(ssym)) => s.ResolvedName(Some(srange), ssym)
          case other => sys.error(s"bad database: unsupported name $other")
        }
        val smessages = mmessages.map {
          case m.Message(mRange(srange), mSeverity(ssym), smessage) => s.Message(Some(srange), ssym, smessage)
          case other => sys.error(s"bad database: unsupported message $other")
        }
        val sdenots = mdenots.map {
          case (mSymbol(ssym), mDenotation(sdenot)) => s.SymbolDenotation(ssym, Some(sdenot))
          case other => sys.error(s"bad database: unsupported denotation $other")
        }
        val ssugars = msugars.map {
          case (mRange(srange), ssyntax) => s.Sugar(Some(srange), ssyntax)
          case other => sys.error(s"bad database: unsupported sugar $other")
        }
        spath -> s.Attributes(scontents, sdialect, snames, smessages, sdenots, ssugars)
      case (other, _) =>
        sys.error(s"unsupported input: $other")
    }
    s.Database(sentries)
  }
}
