package org.langmeta.internal

import java.io.RandomAccessFile
import java.nio.charset.Charset
import java.nio.file.Files
import java.nio.file.OpenOption
import java.nio.file.StandardOpenOption
import scala.util.control.NonFatal
import org.langmeta.inputs.{Input => dInput}
import org.langmeta.inputs.{Position => dPosition}
import org.langmeta.semanticdb.{Synthetic => dSynthetic}
import org.langmeta.internal.io.PathIO
import org.langmeta.internal.semanticdb.{schema => s}
import org.langmeta.internal.semanticdb.{vfs => v}
import org.langmeta.io._
import org.langmeta.semanticdb.Signature
import org.langmeta.{semanticdb => d}

package object semanticdb {
  implicit class XtensionSchemaTextDocuments(sdocuments: s.TextDocuments) {
    def mergeDiagnosticOnlyDocuments: s.TextDocuments = {
      // returns true if this document contains only diagnostics and nothing else.
      // deprecation messages are reported in refchecks and get persisted
      // as standalone documents that need to be merged with their typer-phase
      // document during loading. It seems there's no way to merge the documents
      // during compilation without introducing a lot of memory pressure.
      def isOnlyMessages(sdocument: s.TextDocument): Boolean =
        sdocument.diagnostics.nonEmpty &&
          sdocument.text.isEmpty &&
          sdocument.symbols.isEmpty &&
          sdocument.occurrences.isEmpty &&
          sdocument.synthetics.isEmpty
      if (sdocuments.documents.length <= 1) {
        // NOTE(olafur) the most common case is that there is only a single database
        // per document so we short-circuit here if that's the case.
        sdocuments
      } else {
        sdocuments.documents match {
          case Seq(smaindoc, sdiagdoc)
            if smaindoc.uri == sdiagdoc.uri &&
                isOnlyMessages(sdiagdoc) =>
            val smaindoc1 = smaindoc.addDiagnostics(sdiagdoc.diagnostics: _*)
            s.TextDocuments(smaindoc1 :: Nil)
          case _ => sdocuments
        }
      }
    }
    def toVfs(targetroot: AbsolutePath): v.Database = {
      val ventries = sdocuments.documents.toIterator.map { sentry =>
        // TODO: Would it make sense to support multiclasspaths?
        // One use-case for this would be in-place updates of semanticdb files.
        val vpath = v.SemanticdbPaths.fromScala(RelativePath(sentry.uri))
        val fragment = Fragment(targetroot, vpath)
        val bytes = s.TextDocuments(List(sentry)).toByteArray
        v.Entry.InMemory(fragment, bytes)
      }
      v.Database(ventries.toList)
    }

    def toDb(sourcepath: Option[Sourcepath], sdoc: s.TextDocument): d.Document = {
      val s.TextDocument(sformat, suri, stext, slanguage, ssymbols, soccurrences, sdiagnostics, ssynthetics) = sdoc
      assert(sformat == "semanticdb2", "s.TextDocument.format must be \"semanticdb2\"")
      val dinput = {
        val sfilename = {
          assert(suri.nonEmpty, "s.TextDocument.uri must not be empty")
          PathIO.fromUnix(suri)
        }
        if (stext == "") {
          val duri =
            sourcepath.getOrElse(sys.error("Sourcepath is required to load slim semanticdb."))
                .find(RelativePath(sfilename))
                .getOrElse(sys.error(s"can't find $sfilename in $sourcepath"))
          dInput.File(AbsolutePath(duri.getPath))
        } else {
          dInput.VirtualFile(sfilename.toString, stext)
        }
      }
      object sRange {
        def unapply(srange: s.Range): Option[dPosition] = {
          val dstartOffset = dinput.lineToOffset(srange.startLine) + srange.startCharacter
          val dendOffset = dinput.lineToOffset(srange.endLine) + srange.endCharacter
          Some(dPosition.Range(dinput, dstartOffset, dendOffset))
        }
      }
      object sSeverity {
        def unapply(sseverity: s.Diagnostic.Severity): Option[d.Severity] = {
          sseverity match {
            case s.Diagnostic.Severity.INFO => Some(d.Severity.Info)
            case s.Diagnostic.Severity.WARNING => Some(d.Severity.Warning)
            case s.Diagnostic.Severity.ERROR => Some(d.Severity.Error)
            case _ => None
          }
        }
      }
      object sSymbolInformation {
        def unapply(ssymbolInformation: s.SymbolInformation): Option[d.ResolvedSymbol] = ssymbolInformation match {
          case s.SymbolInformation(d.Symbol(dsym), dflags, dname: String, dsignature: String, soccurrences, smembers, soverrides) =>
            val ddenotInput = dInput.Denotation(dsignature, dsym)
            val dnames = soccurrences.toIterator.map {
              case s.SymbolOccurrence(Some(srange), d.Symbol(dsym), disDefinition) =>
                val dstartOffset = ddenotInput.lineToOffset(srange.startLine) + srange.startCharacter
                val dendOffset = ddenotInput.lineToOffset(srange.endLine) + srange.endCharacter
                val ddefnpos = dPosition.Range(ddenotInput, dstartOffset, dendOffset)
                d.ResolvedName(ddefnpos, dsym, disDefinition)
              case other =>
                sys.error(s"bad protobuf: unsupported occurrence $other")
            }.toList
            val dmembers: List[d.Signature] = smembers.toIterator.map { smember =>
              if (smember.endsWith("#")) d.Signature.Type(smember.stripSuffix("#"))
              else if (smember.endsWith(".")) d.Signature.Term(smember.stripSuffix("."))
              else sys.error(s"Unexpected signature $smember")
            }.toList
            val doverrides = soverrides.flatMap(d.Symbol.unapply).toList
            val ddenot = d.Denotation(dflags, dname, dsignature, dnames, dmembers, doverrides)
            Some(d.ResolvedSymbol(dsym, ddenot))
          case other => sys.error(s"bad protobuf: unsupported symbol information $other")
        }
      }
      object sSynthetic {
        def unapply(ssynthetic: s.Synthetic): Option[dSynthetic] = ssynthetic match {
          case s.Synthetic(Some(sRange(dpos)), dtext, soccurrences) =>
            val dnames = soccurrences.toIterator.map {
              case s.SymbolOccurrence(Some(srange), d.Symbol(dsym), disDefinition) =>
                val dsyntheticinput = dInput.Synthetic(dtext, dpos.input, dpos.start, dpos.end)
                val dstartOffset = dsyntheticinput.lineToOffset(srange.startLine) + srange.startCharacter
                val dendOffset = dsyntheticinput.lineToOffset(srange.endLine) + srange.endCharacter
                val dsyntheticpos = dPosition.Range(dsyntheticinput, dstartOffset, dendOffset)
                d.ResolvedName(dsyntheticpos, dsym, disDefinition)
              case other =>
                sys.error(s"bad protobuf: unsupported occurrence $other")
            }.toList
            Some(dSynthetic(dpos, dtext, dnames))
        }
      }
      val dlanguage = slanguage
      val dnames = soccurrences.map {
        case s.SymbolOccurrence(Some(sRange(dpos)), d.Symbol(dsym), disDefinition) => d.ResolvedName(dpos, dsym, disDefinition)
        case other => sys.error(s"bad protobuf: unsupported occurrence $other")
      }.toList
      val dmessages = sdiagnostics.map {
        case s.Diagnostic(Some(sRange(dpos)), sSeverity(dseverity), dmsg: String) =>
          d.Message(dpos, dseverity, dmsg)
        case other => sys.error(s"bad protobuf: unsupported diagnostic $other")
      }.toList
      val dsymbols = ssymbols.map {
        case sSymbolInformation(dresolvedsymbol) => dresolvedsymbol
      }.toList
      val dsynthetics = ssynthetics.toIterator.map {
        case sSynthetic(dsynthetic) => dsynthetic
        case other => sys.error(s"bad protobuf: unsupported synthetic $other")
      }.toList
      d.Document(dinput, dlanguage, dnames, dmessages, dsymbols, dsynthetics)
    }

    def toDb(sourcepath: Option[Sourcepath]): d.Database = {
      val dentries = sdocuments.documents.toIterator.map { sdoc =>
        try {
          toDb(sourcepath, sdoc)
        } catch {
          case NonFatal(e) =>
            throw new IllegalArgumentException(
              s"Error converting s.TextDocument to m.Document where uri=${sdoc.uri}\n$sdoc",
              e)
        }
      }
      d.Database(dentries.toList)
    }
  }

  implicit class XtensionDatabase(ddatabase: d.Database) {
    def toSchema(sourceroot: AbsolutePath): s.TextDocuments = {
      val sentries = ddatabase.documents.map {
        case d.Document(dinput, dlanguage, dnames, dmessages, dsymbols, dsynthetics) =>
          object dPosition {
            def unapply(dpos: dPosition): Option[s.Range] = dpos match {
              case dpos: org.langmeta.Position.Range =>
                Some(s.Range(dpos.startLine, dpos.startColumn, dpos.endLine, dpos.endColumn))
              case _ =>
                None
            }
          }
          object dSeverity {
            def unapply(dseverity: d.Severity): Option[s.Diagnostic.Severity] = {
              dseverity match {
                case d.Severity.Info => Some(s.Diagnostic.Severity.INFO)
                case d.Severity.Warning => Some(s.Diagnostic.Severity.WARNING)
                case d.Severity.Error => Some(s.Diagnostic.Severity.ERROR)
                case _ => None
              }
            }
          }
          object dResolvedSymbol {
            def unapply(dresolvedSymbol: d.ResolvedSymbol): Option[s.SymbolInformation] = {
              val d.ResolvedSymbol(dsymbol, ddenot) = dresolvedSymbol
              val ssymbol = dsymbol.syntax
              val sflags = ddenot.flags
              val sname = ddenot.name
              val ssignature = ddenot.signature
              val soccurrences = ddenot.names.map {
                case d.ResolvedName(dpos: org.langmeta.Position.Range, ssym, sisDefinition) =>
                  val srange = s.Range(dpos.startLine, dpos.startColumn, dpos.endLine, dpos.endColumn)
                  s.SymbolOccurrence(Some(srange), ssym.syntax, sisDefinition)
                case other =>
                  sys.error(s"bad database: unsupported name $other")
              }
              val smembers = ddenot.members.map(_.syntax)
              val soverrides = ddenot.overrides.map(_.syntax)
              Some(s.SymbolInformation(ssymbol, sflags, sname, ssignature, soccurrences, smembers, soverrides))
            }
          }
          object dSynthetic {
            def unapply(dsynthetic: dSynthetic): Option[s.Synthetic] = dsynthetic match {
              case d.Synthetic(dPosition(srange), ssyntax, dnames) =>
                val soccurrences = dnames.toIterator.map {
                  case d.ResolvedName(dpos: org.langmeta.Position.Range, ssym, sisDefinition) =>
                    val srange = s.Range(dpos.startLine, dpos.startColumn, dpos.endLine, dpos.endColumn)
                    s.SymbolOccurrence(Some(srange), ssym.syntax, sisDefinition)
                  case other =>
                    sys.error(s"bad database: unsupported name $other")
                }.toSeq
                Some(s.Synthetic(Some(srange), ssyntax, soccurrences))
              case _ =>
                None
            }
          }
          val sformat = "semanticdb2"
          val (splatformpath, stext) = dinput match {
            case dInput.File(path, charset) if charset == Charset.forName("UTF-8") =>
              path.toRelative(sourceroot).toString -> ""
            case dInput.VirtualFile(path, value) =>
              path -> value
            case other =>
              sys.error(s"bad database: unsupported input $other")
          }
          val suri = {
            val result = PathIO.toUnix(splatformpath)
            assert(result.nonEmpty, s"'$result'.nonEmpty")
            result
          }
          val slanguage = dlanguage
          val ssymbols = dsymbols.map {
            case dResolvedSymbol(ssymbolInformation) => ssymbolInformation
            case other => sys.error(s"bad database: unsupported denotation $other")
          }
          val soccurrences = dnames.map {
            case d.ResolvedName(dPosition(srange), ssym, sisDefinition) => s.SymbolOccurrence(Some(srange), ssym.syntax, sisDefinition)
            case other => sys.error(s"bad database: unsupported name $other")
          }
          val sdiagnostics = dmessages.map {
            case d.Message(dPosition(srange), dSeverity(ssym), smessage) => s.Diagnostic(Some(srange), ssym, smessage)
            case other => sys.error(s"bad database: unsupported message $other")
          }
          val ssynthetics = dsynthetics.toIterator.map {
            case dSynthetic(ssynthetic) => ssynthetic
            case other => sys.error(s"bad database: unsupported synthetic $other")
          }.toSeq
          s.TextDocument(sformat, suri, stext, slanguage, ssymbols, soccurrences, sdiagnostics, ssynthetics)
      }
      s.TextDocuments(sentries)
    }
  }
}
