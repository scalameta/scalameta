package scala.meta
package semantic

import java.nio.charset.Charset
import org.scalameta.unreachable
import org.scalameta.debug
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.compat.Platform.EOL
import scala.meta.internal.ast.Helpers._
import scala.meta.inputs._
import scala.meta.prettyprinters._
import scala.meta.internal.semantic.{schema => s}
import scala.meta.{semantic => m}
import scala.meta.{Dialect => mDialect}
import scala.meta.inputs.{Input => mInput, Position => mPosition, Point => mPoint}
import scala.meta.parsers.{XtensionParsersDialectInput, XtensionParseDialectInput}

private[meta] trait Api extends Flags {
  implicit class XtensionDatabaseSources(database: Database) {
    def sources: Seq[Source] = database.entries.map { case (input, attrs) => attrs.dialect(input).parse[Source].get }
  }

  implicit class XtensionRefSymbol(ref: Ref)(implicit m: Mirror) {
    def symbol: Symbol = {
      def relevantPosition(tree: Tree): Position = tree match {
        case name1: Name => name1.pos
        case _: Term.This => ???
        case _: Term.Super => ???
        case Term.Select(_, name1) => name1.pos
        case Term.ApplyUnary(_, name1) => name1.pos
        case Type.Select(_, name1) => name1.pos
        case Type.Project(_, name1) => name1.pos
        case Type.Singleton(ref1) => relevantPosition(ref1)
        case Ctor.Ref.Select(_, name1) => name1.pos
        case Ctor.Ref.Project(_, name1) => name1.pos
        case Ctor.Ref.Function(name1) => ???
        case _: Importee.Wildcard => ???
        case Importee.Name(name1) => name1.pos
        case Importee.Rename(name1, _) => name1.pos
        case Importee.Unimport(name1) => name1.pos
        case _ => unreachable(debug(tree.syntax, tree.structure))
      }
      val position = relevantPosition(ref)
      m.database.names.getOrElse(position, sys.error(s"semantic DB doesn't contain $ref"))
    }
  }

  implicit class XtensionSymbolDenotation(sym: Symbol)(implicit m: Mirror) extends HasFlags {
    def denot: Denotation = m.database.denotations.getOrElse(sym, sys.error(s"semantic DB doesn't contain $sym"))
    // NOTE: hasFlag/isXXX methods are added here via `extends HasFlags`
    def flags: Long = denot.flags
    def info: String = denot.info
  }

  implicit class XtensionDatabaseSchema(mdatabase: m.Database) {
    def toSchema: s.Database = {
      val sentries = mdatabase.entries.map {
        case (minput, m.Attributes(mdialect, mnames, mmessages, mdenots, msugars)) =>
          object mDialect {
            def unapply(mdialect: mDialect): Option[String] = {
              val isStandard = scala.meta.Dialect.standards.exists(_._2 == mdialect)
              if (isStandard) Some(mdialect.toString) else None
            }
          }
          object mPosition {
            def unapply(mpos: mPosition): Option[s.Range] = mpos match {
              case scala.meta.inputs.Position.Range(`minput`, mPoint.Offset(_, sstart), mPoint.Offset(_, send)) => Some(s.Range(sstart, send))
              case _ => None
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
          val spath = minput match {
            case mInput.File(spath, charset) if charset == Charset.forName("UTF-8") => spath.value
            case other => sys.error(s"bad database: unsupported input $other")
          }
          val sdialect = mdialect match {
            case mDialect(sdialect) => sdialect
            case other => sys.error(s"bad database: unsupported dialect $other")
          }
          val snames = mnames.map {
            case (mPosition(srange), mSymbol(ssym)) => s.ResolvedName(Some(srange), ssym)
            case other => sys.error(s"bad database: unsupported name $other")
          }
          val smessages = mmessages.map {
            case m.Message(mPosition(srange), mSeverity(ssym), smessage) => s.Message(Some(srange), ssym, smessage)
            case other => sys.error(s"bad database: unsupported message $other")
          }
          val sdenots = mdenots.map {
            case (mSymbol(ssym), mDenotation(sdenot)) => s.SymbolDenotation(ssym, Some(sdenot))
            case other => sys.error(s"bad database: unsupported denotation $other")
          }
          val ssugars = msugars.map {
            case (mPosition(srange), ssyntax) => s.Sugar(Some(srange), ssyntax)
            case other => sys.error(s"bad database: unsupported sugar $other")
          }
          s.Attributes(spath, sdialect, snames, smessages, sdenots, ssugars)
        case (other, _) =>
          sys.error(s"unsupported input: $other")
      }
      s.Database(sentries)
    }
  }
}

private[meta] trait Aliases {
  type Mirror = scala.meta.semantic.Mirror
  val Mirror = scala.meta.semantic.Mirror

  type Database = scala.meta.semantic.Database
  val Database = scala.meta.semantic.Database

  type Attributes = scala.meta.semantic.Attributes
  val Attributes = scala.meta.semantic.Attributes

  type Symbol = scala.meta.semantic.Symbol
  val Symbol = scala.meta.semantic.Symbol

  type Signature = scala.meta.semantic.Signature
  val Signature = scala.meta.semantic.Signature

  type Message = scala.meta.semantic.Message
  val Message = scala.meta.semantic.Message

  type Severity = scala.meta.semantic.Severity
  val Severity = scala.meta.semantic.Severity

  type Denotation = scala.meta.semantic.Denotation
  val Denotation = scala.meta.semantic.Denotation
}
