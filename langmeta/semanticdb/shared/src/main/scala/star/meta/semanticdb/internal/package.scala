package lang.meta.internal

import java.nio.charset.Charset
import lang.meta.inputs.{Input => dInput}
import lang.meta.inputs.{Position => dPosition}
import lang.meta.semanticdb.{Synthetic => dSynthetic}
import lang.meta.internal.io.PathIO
import lang.meta.internal.semanticdb.{schema => s}
import lang.meta.internal.semanticdb.{vfs => v}
import lang.meta.io._
import lang.meta.{semanticdb => d}

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
        case s.Attributes(sunixfilename, scontents, slanguage, snames, smessages, ssymbols, ssynthetics) =>
          assert(sunixfilename.nonEmpty, "s.Attributes.filename must not be empty")
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
          object sPosition {
            def unapply(spos: s.Position): Option[dPosition] = {
              Some(dPosition.Range(dinput, spos.start, spos.end))
            }
          }
          object sSeverity {
            def unapply(sseverity: s.Message.Severity): Option[d.Severity] = {
              sseverity match {
                case s.Message.Severity.INFO => Some(d.Severity.Info)
                case s.Message.Severity.WARNING => Some(d.Severity.Warning)
                case s.Message.Severity.ERROR => Some(d.Severity.Error)
                case _ => None
              }
            }
          }
          object sResolvedSymbol {
            def unapply(sresolvedsymbol: s.ResolvedSymbol): Option[d.ResolvedSymbol] = sresolvedsymbol match {
              case s.ResolvedSymbol(d.Symbol(dsym), Some(s.Denotation(dflags, dname: String, dsignature: String, snames))) =>
                val ddenotinput = dInput.Denotation(dsignature, dsym.syntax)
                val dnames = snames.toIterator.map {
                  case s.ResolvedName(Some(s.Position(sstart, send)), d.Symbol(dsym), disDefinition) =>
                    val ddenotpos = dPosition.Range(ddenotinput, sstart, send)
                    d.ResolvedName(ddenotpos, dsym, disDefinition)
                  case other =>
                    sys.error(s"bad protobuf: unsupported name $other")
                }.toList
                val ddenot = d.Denotation(dflags, dname, dsignature, dnames)
                Some(d.ResolvedSymbol(dsym, ddenot))
              case other => sys.error(s"bad protobuf: unsupported denotation $other")
            }
          }
          object sSugar {
            def unapply(ssugar: s.Sugar): Option[dSynthetic] = ssugar match {
              case s.Sugar(Some(sPosition(dpos)), dtext, snames) =>
                val dnames = snames.toIterator.map {
                  case s.ResolvedName(Some(s.Position(sstart, send)), d.Symbol(dsym), disDefinition) =>
                    val dsugarinput = dInput.Sugar(dtext, dpos.input, dpos.start, dpos.end)
                    val dsugarpos = dPosition.Range(dsugarinput, sstart, send)
                    d.ResolvedName(dsugarpos, dsym, disDefinition)
                  case other =>
                    sys.error(s"bad protobuf: unsupported name $other")
                }.toList
              Some(dSynthetic(dpos, dtext, dnames))
            }
          }
          val dlanguage = slanguage
          val dnames = snames.map {
            case s.ResolvedName(Some(sPosition(dpos)), d.Symbol(dsym), disDefinition) => d.ResolvedName(dpos, dsym, disDefinition)
            case other => sys.error(s"bad protobuf: unsupported name $other")
          }.toList
          val dmessages = smessages.map {
            case s.Message(Some(sPosition(dpos)), sSeverity(dseverity), dmsg: String) =>
              d.Message(dpos, dseverity, dmsg)
            case other => sys.error(s"bad protobuf: unsupported message $other")
          }.toList
          val dsymbols = ssymbols.map {
            case sResolvedSymbol(dresolvedsymbol) => dresolvedsymbol
          }.toList
          val dsynthetics = ssynthetics.toIterator.map {
            case sSugar(dsugar) => dsugar
            case other => sys.error(s"bad protobuf: unsupported sugar $other")
          }.toList
          d.Attributes(dinput, dlanguage, dnames, dmessages, dsymbols, dsynthetics)
      }
      d.Database(dentries.toList)
    }
  }
  implicit class XtensionDatabase(ddatabase: d.Database) {
    def toSchema(sourceroot: AbsolutePath): s.Database = {
      val sentries = ddatabase.entries.map {
        case d.Attributes(dinput, dlanguage, dnames, dmessages, dsymbols, dsynthetics) =>
          object dPosition {
            def unapply(dpos: dPosition): Option[s.Position] = dpos match {
              case lang.meta.inputs.Position.Range(`dinput`, sstart, send) =>
                Some(s.Position(sstart, send))
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
              case d.Denotation(sflags, sname, ssignature, dnames) =>
                val snames = dnames.map {
                  case d.ResolvedName(lang.meta.inputs.Position.Range(_, sstart, send), ssym, sisDefinition) =>
                    s.ResolvedName(Some(s.Position(sstart, send)), ssym.syntax, sisDefinition)
                  case other =>
                    sys.error(s"bad database: unsupported position $other")
                }
                Some(s.Denotation(sflags, sname, ssignature, snames))
              case _ => None
            }
          }
          object dSynthetic {
            def unapply(dsugar: dSynthetic): Option[s.Sugar] = dsugar match {
              case d.Synthetic(dPosition(spos), ssyntax, dnames) =>
                val snames = dnames.toIterator.map {
                  case d.ResolvedName(lang.meta.inputs.Position.Range(_, sstart, send), ssym, sisDefinition) =>
                    s.ResolvedName(Some(s.Position(sstart, send)), ssym.syntax, sisDefinition)
                  case other =>
                    sys.error(s"bad database: unsupported name $other")
                }.toSeq
                Some(s.Sugar(Some(spos), ssyntax, snames))
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
            case d.ResolvedName(dPosition(spos), ssym, sisDefinition) => s.ResolvedName(Some(spos), ssym.syntax, sisDefinition)
            case other => sys.error(s"bad database: unsupported name $other")
          }
          val smessages = dmessages.map {
            case d.Message(dPosition(spos), dSeverity(ssym), smessage) => s.Message(Some(spos), ssym, smessage)
            case other => sys.error(s"bad database: unsupported message $other")
          }
          val ssymbols = dsymbols.map {
            case d.ResolvedSymbol(ssym, dDenotation(sdenot)) => s.ResolvedSymbol(ssym.syntax, Some(sdenot))
            case other => sys.error(s"bad database: unsupported denotation $other")
          }
          val ssynthetics = dsynthetics.toIterator.map {
            case dSynthetic(ssugar) => ssugar
            case other => sys.error(s"bad database: unsupported sugar $other")
          }.toSeq
          s.Attributes(spath, scontents, slanguage, snames, smessages, ssymbols, ssynthetics)
      }
      s.Database(sentries)
    }
  }
}
