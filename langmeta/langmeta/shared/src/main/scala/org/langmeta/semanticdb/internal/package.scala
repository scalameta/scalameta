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
          case s.SymbolInformation(d.Symbol(dsym), _, skind, sproperties, sname, _, ssignature, smembers, soverrides) =>
            val dflags = {
              var dflags = 0L
              def dflip(dbit: Long) = dflags ^= dbit
              skind match {
                case s.SymbolInformation.Kind.UNKNOWN1.value => ()
                case s.SymbolInformation.Kind.VALUE.value => dflip(d.VAL)
                case s.SymbolInformation.Kind.VARIABLE.value => dflip(d.VAR)
                case s.SymbolInformation.Kind.METHOD.value => dflip(d.DEF)
                case s.SymbolInformation.Kind.PRIMARY_CONSTRUCTOR.value => dflip(d.PRIMARYCTOR)
                case s.SymbolInformation.Kind.SECONDARY_CONSTRUCTOR.value => dflip(d.SECONDARYCTOR)
                case s.SymbolInformation.Kind.MACRO.value => dflip(d.MACRO)
                case s.SymbolInformation.Kind.TYPE.value => dflip(d.TYPE)
                case s.SymbolInformation.Kind.PARAMETER.value => dflip(d.PARAM)
                case s.SymbolInformation.Kind.TYPE_PARAMETER.value => dflip(d.TYPEPARAM)
                case s.SymbolInformation.Kind.OBJECT.value => dflip(d.OBJECT)
                case s.SymbolInformation.Kind.PACKAGE.value => dflip(d.PACKAGE)
                case s.SymbolInformation.Kind.PACKAGE_OBJECT.value => dflip(d.PACKAGEOBJECT)
                case s.SymbolInformation.Kind.CLASS.value => dflip(d.CLASS)
                case s.SymbolInformation.Kind.TRAIT.value => dflip(d.TRAIT)
              }
              def stest(bit: Long) = (sproperties & bit) == bit
              if (stest(s.SymbolInformation.Property.PRIVATE.value)) dflip(d.PRIVATE)
              if (stest(s.SymbolInformation.Property.PROTECTED.value)) dflip(d.PROTECTED)
              if (stest(s.SymbolInformation.Property.ABSTRACT.value)) dflip(d.ABSTRACT)
              if (stest(s.SymbolInformation.Property.FINAL.value)) dflip(d.FINAL)
              if (stest(s.SymbolInformation.Property.SEALED.value)) dflip(d.SEALED)
              if (stest(s.SymbolInformation.Property.IMPLICIT.value)) dflip(d.IMPLICIT)
              if (stest(s.SymbolInformation.Property.LAZY.value)) dflip(d.LAZY)
              if (stest(s.SymbolInformation.Property.CASE.value)) dflip(d.CASE)
              if (stest(s.SymbolInformation.Property.COVARIANT.value)) dflip(d.COVARIANT)
              if (stest(s.SymbolInformation.Property.CONTRAVARIANT.value)) dflip(d.CONTRAVARIANT)
              dflags
            }
            val dname = sname
            val dsignature = ssignature.map(_.text).getOrElse("")
            val dnames = {
              ssignature.map { ssignature =>
                val dinput = dInput.Denotation(dsignature, dsym)
                ssignature.occurrences.toIterator.map {
                  case s.SymbolOccurrence(Some(srange), d.Symbol(dsym), disDefinition) =>
                    val dstartOffset = dinput.lineToOffset(srange.startLine) + srange.startCharacter
                    val dendOffset = dinput.lineToOffset(srange.endLine) + srange.endCharacter
                    val ddefnpos = dPosition.Range(dinput, dstartOffset, dendOffset)
                    d.ResolvedName(ddefnpos, dsym, disDefinition)
                  case other =>
                    sys.error(s"bad protobuf: unsupported occurrence $other")
                }.toList
              }.getOrElse(Nil)
            }
            val dmembers: List[d.Signature] = smembers.toIterator.map { smember =>
              if (smember.endsWith("#")) d.Signature.Type(smember.stripSuffix("#"))
              else if (smember.endsWith(".")) d.Signature.Term(smember.stripSuffix("."))
              else sys.error(s"Unexpected signature $smember")
            }.toList
            val doverrides = soverrides.flatMap(d.Symbol.unapply).toList
            Some(d.ResolvedSymbol(dsym, d.Denotation(dflags, dname, dsignature, dnames, dmembers, doverrides)))
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
              val slanguage = dlanguage
              def dtest(bit: Long) = (ddenot.flags & bit) == bit
              val skind = {
                if (dtest(d.VAL)) s.SymbolInformation.Kind.VALUE.value
                else if (dtest(d.VAR)) s.SymbolInformation.Kind.VARIABLE.value
                else if (dtest(d.DEF)) s.SymbolInformation.Kind.METHOD.value
                else if (dtest(d.PRIMARYCTOR)) s.SymbolInformation.Kind.PRIMARY_CONSTRUCTOR.value
                else if (dtest(d.SECONDARYCTOR)) s.SymbolInformation.Kind.SECONDARY_CONSTRUCTOR.value
                else if (dtest(d.MACRO)) s.SymbolInformation.Kind.MACRO.value
                else if (dtest(d.TYPE)) s.SymbolInformation.Kind.TYPE.value
                else if (dtest(d.PARAM)) s.SymbolInformation.Kind.PARAMETER.value
                else if (dtest(d.TYPEPARAM)) s.SymbolInformation.Kind.TYPE_PARAMETER.value
                else if (dtest(d.OBJECT)) s.SymbolInformation.Kind.OBJECT.value
                else if (dtest(d.PACKAGE)) s.SymbolInformation.Kind.PACKAGE.value
                else if (dtest(d.PACKAGEOBJECT)) s.SymbolInformation.Kind.PACKAGE_OBJECT.value
                else if (dtest(d.CLASS)) s.SymbolInformation.Kind.CLASS.value
                else if (dtest(d.TRAIT)) s.SymbolInformation.Kind.TRAIT.value
                else s.SymbolInformation.Kind.UNKNOWN1.value
              }
              val sproperties = {
                var sproperties = 0
                def sflip(sbit: Int) = sproperties ^= sbit
                if (dtest(d.PRIVATE)) sflip(s.SymbolInformation.Property.PRIVATE.value)
                if (dtest(d.PROTECTED)) sflip(s.SymbolInformation.Property.PROTECTED.value)
                if (dtest(d.ABSTRACT)) sflip(s.SymbolInformation.Property.ABSTRACT.value)
                if (dtest(d.FINAL)) sflip(s.SymbolInformation.Property.FINAL.value)
                if (dtest(d.SEALED)) sflip(s.SymbolInformation.Property.SEALED.value)
                if (dtest(d.IMPLICIT)) sflip(s.SymbolInformation.Property.IMPLICIT.value)
                if (dtest(d.LAZY)) sflip(s.SymbolInformation.Property.LAZY.value)
                if (dtest(d.CASE)) sflip(s.SymbolInformation.Property.CASE.value)
                if (dtest(d.COVARIANT)) sflip(s.SymbolInformation.Property.COVARIANT.value)
                if (dtest(d.CONTRAVARIANT)) sflip(s.SymbolInformation.Property.CONTRAVARIANT.value)
                sproperties
              }
              val sname = ddenot.name
              val srange = None
              val ssignature = {
                val stext = ddenot.signature
                val soccurrences = ddenot.names.map {
                  case d.ResolvedName(dpos: org.langmeta.Position.Range, ssym, sisDefinition) =>
                    val srange = s.Range(dpos.startLine, dpos.startColumn, dpos.endLine, dpos.endColumn)
                    s.SymbolOccurrence(Some(srange), ssym.syntax, sisDefinition)
                  case other =>
                    sys.error(s"bad database: unsupported name $other")
                }
                Some(s.TextDocument(text = stext, occurrences = soccurrences))
              }
              val smembers = ddenot.members.map(_.syntax)
              val soverrides = ddenot.overrides.map(_.syntax)
              Some(s.SymbolInformation(ssymbol, slanguage, skind, sproperties, sname, srange, ssignature, smembers, soverrides))
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
