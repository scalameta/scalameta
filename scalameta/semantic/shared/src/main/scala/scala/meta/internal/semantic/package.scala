package scala.meta
package internal

import java.nio.charset.Charset
import java.nio.file.Files
import org.scalameta.invariants.require
import scala.meta.inputs.{Input => mInput}
import scala.meta.inputs.{Position => mPosition}
import scala.meta.semantic.{Sugar => mSugar}
import scala.meta.internal.io.PathIO
import scala.meta.internal.semantic.{schema => s}
import scala.meta.internal.semantic.{vfs => v}
import scala.meta.io._
import scala.meta.{Dialect => mDialect}
import scala.meta.{semantic => m}

package object semantic {

  implicit class XtensionSchemaDatabase(sdatabase: s.Database) {

    def toVfs(targetroot: AbsolutePath): v.Database = {
      val ventries = sdatabase.entries.toIterator.map { sentry =>
        // TODO: Would it make sense to support multiclasspaths?
        // One use-case for this would be in-place updates of semanticdb files.
        val vpath = v.SemanticdbPaths.fromScala(RelativePath(sentry.filename))
        val fragment = Fragment(targetroot, vpath)
        val bytes = s.Database(List(sentry)).toByteArray
        v.Entry.InMemory(fragment, bytes)
      }
      v.Database(ventries.toList)
    }

    def toMeta(sourcepath: Option[Sourcepath]): m.Database = {
      val mentries = sdatabase.entries.toIterator.map {
        case s.Attributes(sunixfilename, scontents, sdialect, snames, smessages, sdenots, ssugars) =>
          assert(sunixfilename.nonEmpty, "s.Attribute.filename must not be empty")
          val sfilename = PathIO.fromUnix(sunixfilename)
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
              Some(mPosition.Range(minput, srange.start, srange.end))
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
          object sSugar {
            def unapply(ssugar: s.Sugar): Option[(mPosition, mSugar)] = ssugar match {
              case s.Sugar(Some(srange @ sRange(mpos)), syntax, snames) =>
              require(ssugar.syntax.nonEmpty)
              val sugarinput = mInput.Sugar(syntax, minput, srange.start, srange.end)
                val mnames = snames.toIterator.map {
                  case s.ResolvedName(Some(s.Range(sstart, send)), m.Symbol(msym)) =>
                    val sugarpos = mPosition.Range(sugarinput, sstart, send)
                    sugarpos -> msym
                }.toList
              Some(mpos -> mSugar(sugarinput, mnames))
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
            case sSugar(mpos, msugar) => mpos -> msugar
            case other => sys.error(s"bad protobuf: unsupported sugar $other")
          }.toList
          m.Attributes(minput, mdialect, mnames, mmessages, mdenots, msugars)
      }
      m.Database(mentries.toList)
    }
  }
  implicit class XtensionMetaDatabase(mdatabase: m.Database) {
    def toSchema(sourceroot: AbsolutePath): s.Database = {
      val sentries = mdatabase.entries.map {
        case m.Attributes(minput, mdialect, mnames, mmessages, mdenots, msugars) =>
          object mRange {
            def unapply(mpos: mPosition): Option[s.Range] = mpos match {
              case mPosition.Range(`minput`, sstart, send) =>
                Some(s.Range(sstart, send))
              case _ =>
                None
            }
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
          object mPositionSugar {
            def unapply(msugar: (mPosition, mSugar)): Option[s.Sugar] = msugar match {
              case (mRange(srange), mSugar(mInput.Sugar(syntax, _, _, _), mnames)) =>
                val snames = mnames.map {
                  case (mPosition.Range(_: mInput.Sugar, sstart, send), msymbol) =>
                    s.ResolvedName(Some(s.Range(sstart, send)), msymbol.syntax)
                }
                Some(s.Sugar(Some(srange), syntax, snames))
              case _ =>
                None
            }
          }
          val (splatformpath, scontents) = minput match {
            case mInput.File(path, charset) if charset == Charset.forName("UTF-8") =>
              path.toRelative(sourceroot).toString -> ""
            case mInput.LabeledString(label, contents) =>
              label -> contents
            case other =>
              sys.error(s"bad database: unsupported input $other")
          }
          val spath = PathIO.toUnix(splatformpath)
          assert(spath.nonEmpty, s"'$spath'.nonEmpty")
          val sdialect = {
            val sdialect = mDialect.standards.find(_._2 == mdialect).map(_._1)
            sdialect.getOrElse(sys.error(s"bad database: unsupported dialect $mdialect"))
          }
          val snames = mnames.map {
            case (mRange(srange), ssym) => s.ResolvedName(Some(srange), ssym.syntax)
            case other => sys.error(s"bad database: unsupported name $other")
          }
          val smessages = mmessages.map {
            case m.Message(mRange(srange), mSeverity(ssym), smessage) =>
              s.Message(Some(srange), ssym, smessage)
            case other => sys.error(s"bad database: unsupported message $other")
          }
          val sdenots = mdenots.map {
            case (ssym, mDenotation(sdenot)) => s.SymbolDenotation(ssym.syntax, Some(sdenot))
            case other => sys.error(s"bad database: unsupported denotation $other")
          }
          val ssugars = msugars.map {
            case mPositionSugar(ssugar) => ssugar
            case other => sys.error(s"bad database: unsupported sugar $other")
          }
          s.Attributes(spath, scontents, sdialect, snames, smessages, sdenots, ssugars)
      }
      s.Database(sentries)
    }
  }
}
