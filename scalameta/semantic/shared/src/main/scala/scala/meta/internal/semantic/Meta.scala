package scala.meta
package internal
package semantic

import java.nio.charset.Charset
import org.scalameta.data._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.util.Try
import scala.meta.io._
import scala.meta.internal.io._
import scala.meta.internal.semantic.{schema => s}
import scala.meta.{semantic => m}
import scala.meta.{Dialect => mDialect}
import scala.meta.inputs.{Input => mInput, Position => mPosition, Point => mPoint}

package object meta {
  implicit class XtensionDatabaseSchema(mdatabase: m.Database) {
    def toSchema(sourcepath: Sourcepath): s.Database = {
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
}