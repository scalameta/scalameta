package star.meta.internal

import java.nio.charset.Charset
import star.meta.inputs.{Input => dInput}
import star.meta.inputs.{Position => dPosition}
import star.meta.semanticdb.{Sugar => dSugar}
import star.meta.internal.io.PathIO
import star.meta.internal.semanticdb.{schema => s}
import star.meta.internal.semanticdb.{vfs => v}
import star.meta.io._
import star.meta.{semanticdb => d}

package object semanticdb {
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

    def toDb(sourcepath: Option[Sourcepath]): d.Database = {
      val dentries = sdatabase.entries.toIterator.map {
        case s.Attributes(sunixfilename, scontents, slanguage, snames, smessages, sdenots, ssugars) =>
          assert(sunixfilename.nonEmpty, "s.Attribute.filename must not be empty")
          val sfilename = PathIO.fromUnix(sunixfilename)
          val dinput = {
            if (scontents == "") {
              val uri =
                sourcepath.getOrElse(sys.error("Sourcepath is required to load slim semanticdb."))
                    .find(RelativePath(sfilename))
                    .getOrElse(sys.error(s"can't find $sfilename in $sourcepath"))
              dInput.File(AbsolutePath(uri.getPath))
            } else {
              dInput.VirtualFile(sfilename.toString, scontents)
            }
          }
          object sRange {
            def unapply(srange: s.Range): Option[dPosition] = {
              Some(dPosition.Range(dinput, srange.start, srange.end))
            }
          }
          object sSeverity {
            def unapply(ssev: s.Message.Severity): Option[d.Severity] = {
              ssev match {
                case s.Message.Severity.INFO => Some(d.Severity.Info)
                case s.Message.Severity.WARNING => Some(d.Severity.Warning)
                case s.Message.Severity.ERROR => Some(d.Severity.Error)
                case _ => None
              }
            }
          }
          object sDenotation {
            def unapply(sdenot: s.Denotation): Option[d.Denotation] = sdenot match {
              case s.Denotation(dflags, dname: String, minfo: String) =>
                Some(d.Denotation(dflags, dname, minfo))
              case _ => None
            }
          }
          object sSugar {
            def unapply(ssugar: s.Sugar): Option[(dPosition, dSugar)] = ssugar match {
              case s.Sugar(Some(srange @ sRange(dpos)), syntax, snames) =>
              val sugarinput = dInput.Sugar(syntax, dinput, srange.start, srange.end)
                val dnames = snames.toIterator.map {
                  case s.ResolvedName(Some(s.Range(sstart, send)), d.Symbol(msym)) =>
                    val sugarpos: dPosition = dPosition.Range(sugarinput, sstart, send)
                    sugarpos -> msym
                }.toMap
              Some(dpos -> dSugar(sugarinput, dnames))
            }
          }
          val dlanguage = slanguage
          val dnames = snames.map {
            case s.ResolvedName(Some(sRange(dpos)), d.Symbol(msym)) => dpos -> msym
            case other => sys.error(s"bad protobuf: unsupported name $other")
          }.toList
          val dmessages = smessages.map {
            case s.Message(Some(sRange(dpos)), sSeverity(dseverity), mmsg: String) =>
              d.Message(dpos, dseverity, mmsg)
            case other => sys.error(s"bad protobuf: unsupported message $other")
          }.toList
          val ddenots = sdenots.map {
            case s.SymbolDenotation(d.Symbol(msym), Some(sDenotation(ddenot))) => msym -> ddenot
            case other => sys.error(s"bad protobuf: unsupported denotation $other")
          }.toList
          val dsugars = ssugars.toIterator.map {
            case sSugar(dpos, dsugar) => dpos -> dsugar
            case other => sys.error(s"bad protobuf: unsupported sugar $other")
          }.toMap
          d.Attributes(dinput, dlanguage, dnames, dmessages, ddenots, dsugars)
      }
      d.Database(dentries.toList)
    }
  }
  implicit class XtensionDatabase(ddatabase: d.Database) {
    def toSchema(sourceroot: AbsolutePath): s.Database = {
      val sentries = ddatabase.entries.map {
        case d.Attributes(dinput, dlanguage, dnames, dmessages, ddenots, dsugars) =>
          object dRange {
            def unapply(dpos: dPosition): Option[s.Range] = dpos match {
              case dPosition.Range(`dinput`, sstart, send) =>
                Some(s.Range(sstart, send))
              case _ =>
                None
            }
          }
          object dSeverity {
            def unapply(dseverity: d.Severity): Option[s.Message.Severity] = {
              dseverity match {
                case d.Severity.Info => Some(s.Message.Severity.INFO)
                case d.Severity.Warning => Some(s.Message.Severity.WARNING)
                case d.Severity.Error => Some(s.Message.Severity.ERROR)
                case _ => None
              }
            }
          }
          object dDenotation {
            def unapply(ddenot: d.Denotation): Option[s.Denotation] = ddenot match {
              case d.Denotation(sflags, sname, sinfo) => Some(s.Denotation(sflags, sname, sinfo))
              case _ => None
            }
          }
          object dPositionSugar {
            def unapply(dpossugar: (dPosition, dSugar)): Option[s.Sugar] = dpossugar match {
              case (dRange(srange), dSugar(dInput.Sugar(syntax, _, _, _), dnames)) =>
                val snames = dnames.toIterator.map {
                  case (dPosition.Range(_: dInput.Sugar, sstart, send), dsymbol) =>
                    s.ResolvedName(Some(s.Range(sstart, send)), dsymbol.syntax)
                  case other =>
                    sys.error(s"bad database: unsupported name $other")
                }.toSeq
                Some(s.Sugar(Some(srange), syntax, snames))
              case _ =>
                None
            }
          }
          val (splatformpath, scontents) = dinput match {
            case dInput.File(path, charset) if charset == Charset.forName("UTF-8") =>
              path.toRelative(sourceroot).toString -> ""
            case dInput.VirtualFile(path, contents) =>
              path -> contents
            case other =>
              sys.error(s"bad database: unsupported input $other")
          }
          val spath = PathIO.toUnix(splatformpath)
          assert(spath.nonEmpty, s"'$spath'.nonEmpty")
          val slanguage = dlanguage
          val snames = dnames.map {
            case (dRange(srange), ssym) => s.ResolvedName(Some(srange), ssym.syntax)
            case other => sys.error(s"bad database: unsupported name $other")
          }
          val smessages = dmessages.map {
            case d.Message(dRange(srange), dSeverity(ssym), smessage) =>
              s.Message(Some(srange), ssym, smessage)
            case other => sys.error(s"bad database: unsupported message $other")
          }
          val sdenots = ddenots.map {
            case (ssym, dDenotation(sdenot)) => s.SymbolDenotation(ssym.syntax, Some(sdenot))
            case other => sys.error(s"bad database: unsupported denotation $other")
          }
          val ssugars = dsugars.toIterator.map {
            case dPositionSugar(ssugar) => ssugar
            case other => sys.error(s"bad database: unsupported sugar $other")
          }.toSeq
          s.Attributes(spath, scontents, slanguage, snames, smessages, sdenots, ssugars)
      }
      s.Database(sentries)
    }
  }
}
