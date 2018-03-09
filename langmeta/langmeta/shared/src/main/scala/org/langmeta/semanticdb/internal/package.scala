package org.langmeta.internal

import java.nio.charset.Charset
import scala.util.control.NonFatal
import org.langmeta.inputs.{Input => dInput}
import org.langmeta.inputs.{Position => dPosition}
import org.langmeta.semanticdb.{Synthetic => dSynthetic}
import org.langmeta.internal.io.PathIO
import org.langmeta.internal.semanticdb.{vfs => v}
import org.langmeta.io._
import org.langmeta.{semanticdb => d}
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.Accessibility.{Tag => a}
import scala.meta.internal.semanticdb3.{Language => l}
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}

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
      val s.TextDocument(sschema, suri, stext, slanguage, ssymbols, soccurrences, sdiagnostics, ssynthetics) = sdoc
      assert(sschema == s.Schema.SEMANTICDB3, "s.TextDocument.schema must be ${s.Schema.SEMANTICDB3}")
      val symbolSuffix = localSymbolSuffix(suri)
      def dSymbol(ssymbol: String) = {
        if (ssymbol.startsWith("local")) d.Symbol(ssymbol + symbolSuffix)
        else d.Symbol(ssymbol)
      }
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
      object sRole {
        def unapply(srole: s.SymbolOccurrence.Role): Option[Boolean] = srole match {
          case s.SymbolOccurrence.Role.REFERENCE => Some(false)
          case s.SymbolOccurrence.Role.DEFINITION => Some(true)
          case _ => None
        }
      }
      object sSeverity {
        def unapply(sseverity: s.Diagnostic.Severity): Option[d.Severity] = {
          sseverity match {
            case s.Diagnostic.Severity.ERROR => Some(d.Severity.Error)
            case s.Diagnostic.Severity.WARNING => Some(d.Severity.Warning)
            case s.Diagnostic.Severity.INFORMATION => Some(d.Severity.Info)
            case s.Diagnostic.Severity.HINT => Some(d.Severity.Hint)
            case _ => None
          }
        }
      }
      object sSymbolInformation {
        def unapply(ssymbolInformation: s.SymbolInformation): Option[d.ResolvedSymbol] = ssymbolInformation match {
          case s.SymbolInformation(ssym, slanguage, skind, sproperties, sname, _, ssignature, smembers, soverrides, stpe, sanns, sacc, sowner) =>
            val dsym = dSymbol(ssym)
            val dflags = {
              var dflags = 0L
              def dflip(dbit: Long) = dflags ^= dbit
              if (slanguage == l.JAVA) dflip(d.JAVADEFINED)
              skind match {
                case k.LOCAL => dflip(d.LOCAL)
                case k.FIELD => dflip(d.FIELD)
                case k.METHOD => dflip(d.METHOD)
                case k.PRIMARY_CONSTRUCTOR => dflip(d.PRIMARYCTOR)
                case k.SECONDARY_CONSTRUCTOR => dflip(d.SECONDARYCTOR)
                case k.MACRO => dflip(d.MACRO)
                case k.TYPE => dflip(d.TYPE)
                case k.PARAMETER => dflip(d.PARAM)
                case k.SELF_PARAMETER => dflip(d.SELFPARAM)
                case k.TYPE_PARAMETER => dflip(d.TYPEPARAM)
                case k.OBJECT => dflip(d.OBJECT)
                case k.PACKAGE => dflip(d.PACKAGE)
                case k.PACKAGE_OBJECT => dflip(d.PACKAGEOBJECT)
                case k.CLASS => dflip(d.CLASS)
                case k.TRAIT => dflip(d.TRAIT)
                case k.INTERFACE => dflip(d.INTERFACE)
                case _ => ()
              }
              def stest(bit: Long) = (sproperties & bit) == bit
              if (stest(p.ABSTRACT.value)) dflip(d.ABSTRACT)
              if (stest(p.FINAL.value)) dflip(d.FINAL)
              if (stest(p.SEALED.value)) dflip(d.SEALED)
              if (stest(p.IMPLICIT.value)) dflip(d.IMPLICIT)
              if (stest(p.LAZY.value)) dflip(d.LAZY)
              if (stest(p.CASE.value)) dflip(d.CASE)
              if (stest(p.COVARIANT.value)) dflip(d.COVARIANT)
              if (stest(p.CONTRAVARIANT.value)) dflip(d.CONTRAVARIANT)
              if (stest(p.VAL.value)) dflip(d.VAL)
              if (stest(p.VAR.value)) dflip(d.VAR)
              sacc.map(_.tag) match {
                case Some(a.PRIVATE | a.PRIVATE_THIS | a.PRIVATE_WITHIN) =>
                  dflip(d.PRIVATE)
                case Some(a.PROTECTED | a.PROTECTED_THIS | a.PROTECTED_WITHIN) =>
                  dflip(d.PROTECTED)
                case _ =>
                  ()
              }
              dflags
            }
            val dname = sname
            val dsignature = ssignature.map(_.text).getOrElse("")
            val dnames = {
              ssignature.map { ssignature =>
                val dinput = dInput.Denotation(dsignature, dsym)
                ssignature.occurrences.toIterator.map {
                  case s.SymbolOccurrence(Some(srange), ssym, sRole(disDefinition)) =>
                    val dstartOffset = dinput.lineToOffset(srange.startLine) + srange.startCharacter
                    val dendOffset = dinput.lineToOffset(srange.endLine) + srange.endCharacter
                    val ddefnpos = dPosition.Range(dinput, dstartOffset, dendOffset)
                    val dsym = dSymbol(ssym)
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
            val doverrides = soverrides.map(dSymbol).toList
            val dtpe = stpe
            val danns = sanns.toList
            val dacc = sacc
            val downer = dSymbol(sowner)
            Some(d.ResolvedSymbol(dsym, d.Denotation(dflags, dname, dsignature, dnames, dmembers, doverrides, dtpe, danns, dacc, downer)))
          case other =>
            sys.error(s"bad protobuf: unsupported symbol information $other")
        }
      }
      object sSynthetic {
        def unapply(ssynthetic: s.Synthetic): Option[dSynthetic] = ssynthetic match {
          case s.Synthetic(Some(sRange(dpos)), stext) =>
            val dtext = stext.map(_.text).getOrElse("")
            val dnames = {
              stext.map { stext =>
                stext.occurrences.toIterator.map {
                  case s.SymbolOccurrence(Some(srange), ssym, sRole(disDefinition)) =>
                    val dsyntheticinput = dInput.Synthetic(dtext, dpos.input, dpos.start, dpos.end)
                    val dstartOffset = dsyntheticinput.lineToOffset(srange.startLine) + srange.startCharacter
                    val dendOffset = dsyntheticinput.lineToOffset(srange.endLine) + srange.endCharacter
                    val dsyntheticpos = dPosition.Range(dsyntheticinput, dstartOffset, dendOffset)
                    val dsym = dSymbol(ssym)
                    d.ResolvedName(dsyntheticpos, dsym, disDefinition)
                  case other =>
                    sys.error(s"bad protobuf: unsupported occurrence $other")
                }.toList
              }.getOrElse(Nil)
            }
            Some(dSynthetic(dpos, dtext, dnames))
        }
      }
      val dlanguage = {
        slanguage match {
          case l.SCALA => "Scala"
          case l.JAVA => "Java"
          case _ => ""
        }
      }
      val dnames = soccurrences.map {
        case s.SymbolOccurrence(Some(sRange(dpos)), ssym, sRole(disDefinition)) =>
          val dsym = dSymbol(ssym)
          d.ResolvedName(dpos, dsym, disDefinition)
        case other =>
          sys.error(s"bad protobuf: unsupported occurrence $other")
      }.toList
      val dmessages = sdiagnostics.map {
        case s.Diagnostic(Some(sRange(dpos)), sSeverity(dseverity), dmsg: String) =>
          d.Message(dpos, dseverity, dmsg)
        case other =>
          sys.error(s"bad protobuf: unsupported diagnostic $other")
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
          val sschema = s.Schema.SEMANTICDB3
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
          val symbolSuffix = localSymbolSuffix(suri)
          def sSymbol(sym: d.Symbol): String = sym match {
            case d.Symbol.Local(id) => id.stripSuffix(symbolSuffix)
            case _ => sym.syntax
          }
          val slanguage = {
            if (dlanguage.startsWith("Scala")) l.SCALA
            else if (dlanguage.startsWith("Java")) l.JAVA
            else l.UNKNOWN_LANGUAGE
          }
          object dPosition {
            def unapply(dpos: dPosition): Option[s.Range] = dpos match {
              case dpos: org.langmeta.Position.Range =>
                Some(s.Range(dpos.startLine, dpos.startColumn, dpos.endLine, dpos.endColumn))
              case _ =>
                None
            }
          }
          object disDefinition {
            def unapply(disDefinition: Boolean): Option[s.SymbolOccurrence.Role] = {
              if (disDefinition) Some(s.SymbolOccurrence.Role.DEFINITION)
              else Some(s.SymbolOccurrence.Role.REFERENCE)
            }
          }
          object dSeverity {
            def unapply(dseverity: d.Severity): Option[s.Diagnostic.Severity] = {
              dseverity match {
                case d.Severity.Error => Some(s.Diagnostic.Severity.ERROR)
                case d.Severity.Warning => Some(s.Diagnostic.Severity.WARNING)
                case d.Severity.Info => Some(s.Diagnostic.Severity.INFORMATION)
                case d.Severity.Hint => Some(s.Diagnostic.Severity.HINT)
                case _ => None
              }
            }
          }
          object dResolvedSymbol {
            def unapply(dresolvedSymbol: d.ResolvedSymbol): Option[s.SymbolInformation] = {
              val d.ResolvedSymbol(dsymbol, ddenot) = dresolvedSymbol
              def dtest(bit: Long) = (ddenot.flags & bit) == bit
              val ssymbol = sSymbol(dsymbol)
              val ssymbolLanguage = if (dtest(d.JAVADEFINED)) l.JAVA else l.SCALA
              val skind = {
                if (dtest(d.LOCAL)) k.LOCAL
                else if (dtest(d.FIELD)) k.FIELD
                else if (dtest(d.METHOD)) k.METHOD
                else if (dtest(d.PRIMARYCTOR)) k.PRIMARY_CONSTRUCTOR
                else if (dtest(d.SECONDARYCTOR)) k.SECONDARY_CONSTRUCTOR
                else if (dtest(d.MACRO)) k.MACRO
                else if (dtest(d.TYPE)) k.TYPE
                else if (dtest(d.PARAM)) k.PARAMETER
                else if (dtest(d.SELFPARAM)) k.SELF_PARAMETER
                else if (dtest(d.TYPEPARAM)) k.TYPE_PARAMETER
                else if (dtest(d.OBJECT)) k.OBJECT
                else if (dtest(d.PACKAGE)) k.PACKAGE
                else if (dtest(d.PACKAGEOBJECT)) k.PACKAGE_OBJECT
                else if (dtest(d.CLASS)) k.CLASS
                else if (dtest(d.TRAIT)) k.TRAIT
                else if (dtest(d.INTERFACE)) k.INTERFACE
                else k.UNKNOWN_KIND
              }
              val sproperties = {
                var sproperties = 0
                def sflip(sprop: s.SymbolInformation.Property) = sproperties ^= sprop.value
                if (dtest(d.ABSTRACT)) sflip(p.ABSTRACT)
                if (dtest(d.FINAL)) sflip(p.FINAL)
                if (dtest(d.SEALED)) sflip(p.SEALED)
                if (dtest(d.IMPLICIT)) sflip(p.IMPLICIT)
                if (dtest(d.LAZY)) sflip(p.LAZY)
                if (dtest(d.CASE)) sflip(p.CASE)
                if (dtest(d.COVARIANT)) sflip(p.COVARIANT)
                if (dtest(d.CONTRAVARIANT)) sflip(p.CONTRAVARIANT)
                if (dtest(d.VAL)) sflip(p.VAL)
                if (dtest(d.VAR)) sflip(p.VAR)
                sproperties
              }
              val sname = ddenot.name
              val slocation = None
              val ssignature = {
                if (ddenot.signature.nonEmpty) {
                  val stext = ddenot.signature
                  val soccurrences = ddenot.names.map {
                    case d.ResolvedName(dpos: org.langmeta.Position.Range, dsym, disDefinition(srole)) =>
                      val srange = s.Range(dpos.startLine, dpos.startColumn, dpos.endLine, dpos.endColumn)
                      val ssym = sSymbol(dsym)
                      s.SymbolOccurrence(Some(srange), ssym, srole)
                    case other =>
                      sys.error(s"bad database: unsupported name $other")
                  }
                  Some(s.TextDocument(text = stext, occurrences = soccurrences))
                } else {
                  None
                }
              }
              val smembers = ddenot.members.map(_.syntax)
              val soverrides = ddenot.overrides.map(sSymbol)
              val stpe = ddenot.tpe
              val sanns = ddenot.annotations
              val sacc = ddenot.accessibility
              val sowner = ddenot.owner.syntax
              Some(s.SymbolInformation(ssymbol, ssymbolLanguage, skind, sproperties, sname, slocation, ssignature, smembers, soverrides, stpe, sanns, sacc, sowner))
            }
          }
          object dSynthetic {
            def unapply(dsynthetic: dSynthetic): Option[s.Synthetic] = dsynthetic match {
              case d.Synthetic(dPosition(srange), ssyntax, dnames) =>
                val stext = {
                  val stext = ssyntax
                  val soccurrences = dnames.toIterator.map {
                    case d.ResolvedName(dpos: org.langmeta.Position.Range, dsym, disDefinition(srole)) =>
                      val srange = s.Range(dpos.startLine, dpos.startColumn, dpos.endLine, dpos.endColumn)
                      val ssym = sSymbol(dsym)
                      s.SymbolOccurrence(Some(srange), ssym, srole)
                    case other =>
                      sys.error(s"bad database: unsupported name $other")
                  }.toSeq
                  Some(s.TextDocument(text = stext, occurrences = soccurrences))
                }
                Some(s.Synthetic(Some(srange), stext))
              case _ =>
                None
            }
          }
          val ssymbols = dsymbols.map {
            case dResolvedSymbol(ssymbolInformation) => ssymbolInformation
            case other => sys.error(s"bad database: unsupported denotation $other")
          }
          val soccurrences = dnames.map {
            case d.ResolvedName(dPosition(srange), dsym, disDefinition(srole)) =>
              val ssym = sSymbol(dsym)
              s.SymbolOccurrence(Some(srange), ssym, srole)
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
          s.TextDocument(sschema, suri, stext, slanguage, ssymbols, soccurrences, sdiagnostics, ssynthetics)
      }
      s.TextDocuments(sentries)
    }
  }

  // Returns suffix to make local symbol unique globally. By default, local symbols
  // from different source files can have conflicting IDs. To distinguish them, we
  // append the URI to the ID.
  private def localSymbolSuffix(uri: String): String = {
    "_" + uri.replaceAll("[^A-Za-z0-9]", "_")
  }
}
