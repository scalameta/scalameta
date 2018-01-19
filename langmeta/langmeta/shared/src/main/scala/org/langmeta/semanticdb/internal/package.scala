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
  implicit class XtensionSchemaDatabase(sdatabase: s.Database) {

    def append(relpath: RelativePath, targetroot: AbsolutePath): Unit = {
      write(relpath, targetroot, StandardOpenOption.CREATE, StandardOpenOption.APPEND)
    }
    def save(relpath: RelativePath, targetroot: AbsolutePath): Unit = {
      write(relpath, targetroot, StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING)
    }
    private def write(
        relpath: RelativePath,
        targetroot: AbsolutePath,
        openOptions: OpenOption*): Unit = {
      val semanticdbPath = targetroot.resolve(v.SemanticdbPaths.fromScala(relpath)).toNIO
      Files.createDirectories(semanticdbPath.getParent)
      val out = Files.newOutputStream(semanticdbPath, openOptions: _*)
      try sdatabase.writeTo(out)
      finally out.close()
    }

    def mergeMessageOnlyDocuments: s.Database = {
      // returns true if this document contains only messages and nothing else.
      // deprecation messages are reported in refchecks and get persisted
      // as standalone documents that need to be merged with their typer-phase
      // document during loading. It seems there's no way to merge the documents
      // during compilation without introducing a lot of memory pressure.
      def isOnlyMessages(sdocument: s.Document): Boolean =
        sdocument.messages.nonEmpty &&
          sdocument.contents.isEmpty &&
          sdocument.names.isEmpty &&
          sdocument.synthetics.isEmpty &&
          sdocument.symbols.isEmpty
      if (sdatabase.documents.length <= 1) {
        // NOTE(olafur) the most common case is that there is only a single database
        // per document so we short-circuit here if that's the case.
        sdatabase
      } else {
        sdatabase.documents match {
          case Seq(doc, messages)
              if doc.filename == messages.filename &&
                isOnlyMessages(messages) =>
            val x = doc.addMessages(messages.messages: _*)
            s.Database(x :: Nil)
          case _ => sdatabase
        }
      }
    }
    def toVfs(targetroot: AbsolutePath): v.Database = {
      val ventries = sdatabase.documents.toIterator.map { sentry =>
        // TODO: Would it make sense to support multiclasspaths?
        // One use-case for this would be in-place updates of semanticdb files.
        val vpath = v.SemanticdbPaths.fromScala(RelativePath(sentry.filename))
        val fragment = Fragment(targetroot, vpath)
        val bytes = s.Database(List(sentry)).toByteArray
        v.Entry.InMemory(fragment, bytes)
      }
      v.Database(ventries.toList)
    }

    def toDb(sourcepath: Option[Sourcepath], sdoc: s.Document): d.Document = {
      val s.Document(
        sunixfilename,
        scontents,
        slanguage,
        snames,
        smessages,
        ssymbols,
        ssynthetics) = sdoc
      assert(sunixfilename.nonEmpty, "s.Document.filename must not be empty")
      val sfilename = PathIO.fromUnix(sunixfilename)
      val dinput = {
        if (scontents == "") {
          val uri =
            sourcepath
              .getOrElse(sys.error("Sourcepath is required to load slim semanticdb."))
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
        def unapply(sresolvedsymbol: s.ResolvedSymbol): Option[d.ResolvedSymbol] =
          sresolvedsymbol match {
            case s.ResolvedSymbol(
                d.Symbol(dsym),
                Some(s.Denotation(dflags, dname: String, dsignature: String, snames, smembers))) =>
              val ddefninput = dInput.Denotation(dsignature, dsym)
              val dnames = snames.toIterator.map {
                case s.ResolvedName(
                    Some(s.Position(sstart, send)),
                    d.Symbol(dsym),
                    disDefinition) =>
                  val ddefnpos = dPosition.Range(ddefninput, sstart, send)
                  d.ResolvedName(ddefnpos, dsym, disDefinition)
                case other =>
                  sys.error(s"bad protobuf: unsupported name $other")
              }.toList
              val dmembers: List[d.Signature] = smembers.toIterator.map { smember =>
                if (smember.endsWith("#")) d.Signature.Type(smember.stripSuffix("#"))
                else if (smember.endsWith(".")) d.Signature.Term(smember.stripSuffix("."))
                else sys.error(s"Unexpected signature $smember")
              }.toList
              val ddefn = d.Denotation(dflags, dname, dsignature, dnames, dmembers)
              Some(d.ResolvedSymbol(dsym, ddefn))
            case other => sys.error(s"bad protobuf: unsupported denotation $other")
          }
      }
      object sSynthetic {
        def unapply(ssynthetic: s.Synthetic): Option[dSynthetic] = ssynthetic match {
          case s.Synthetic(Some(sPosition(dpos)), dtext, snames) =>
            val dnames = snames.toIterator.map {
              case s.ResolvedName(Some(s.Position(sstart, send)), d.Symbol(dsym), disDefinition) =>
                val dsyntheticinput = dInput.Synthetic(dtext, dpos.input, dpos.start, dpos.end)
                val dsyntheticpos = dPosition.Range(dsyntheticinput, sstart, send)
                d.ResolvedName(dsyntheticpos, dsym, disDefinition)
              case other =>
                sys.error(s"bad protobuf: unsupported name $other")
            }.toList
            Some(dSynthetic(dpos, dtext, dnames))
        }
      }
      val dlanguage = slanguage
      val dnames = snames.map {
        case s.ResolvedName(Some(sPosition(dpos)), d.Symbol(dsym), disDefinition) =>
          d.ResolvedName(dpos, dsym, disDefinition)
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
        case sSynthetic(dsynthetic) => dsynthetic
        case other => sys.error(s"bad protobuf: unsupported synthetic $other")
      }.toList
      d.Document(dinput, dlanguage, dnames, dmessages, dsymbols, dsynthetics)
    }

    def toDb(sourcepath: Option[Sourcepath]): d.Database = {
      val dentries = sdatabase.documents.toIterator.map { sdoc =>
        try {
          toDb(sourcepath, sdoc)
        } catch {
          case NonFatal(e) =>
            throw new IllegalArgumentException(
              s"Error converting s.Document to m.Document where filename=${sdoc.filename}\n$sdoc",
              e)
        }
      }
      d.Database(dentries.toList)
    }
  }
  implicit class XtensionDatabase(ddatabase: d.Database) {
    def toSchema(sourceroot: AbsolutePath): s.Database = {
      val sentries = ddatabase.documents.map {
        case d.Document(dinput, dlanguage, dnames, dmessages, dsymbols, dsynthetics) =>
          object dPosition {
            def unapply(dpos: dPosition): Option[s.Position] = dpos match {
              case org.langmeta.inputs.Position.Range(_, sstart, send) =>
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
            def unapply(ddefn: d.Denotation): Option[s.Denotation] = {
              import ddefn._
              val snames = ddefn.names.map {
                case d.ResolvedName(
                    org.langmeta.inputs.Position.Range(_, sstart, send),
                    ssym,
                    sisDefinition) =>
                  s.ResolvedName(Some(s.Position(sstart, send)), ssym.syntax, sisDefinition)
                case other =>
                  sys.error(s"bad database: unsupported position $other")
              }
              val smembers = ddefn.members.map(_.syntax)
              Some(s.Denotation(flags, name, signature, snames, smembers))
            }
          }
          object dSynthetic {
            def unapply(dsynthetic: dSynthetic): Option[s.Synthetic] = dsynthetic match {
              case d.Synthetic(dPosition(spos), ssyntax, dnames) =>
                val snames = dnames.toIterator.map {
                  case d.ResolvedName(
                      org.langmeta.inputs.Position.Range(_, sstart, send),
                      ssym,
                      sisDefinition) =>
                    s.ResolvedName(Some(s.Position(sstart, send)), ssym.syntax, sisDefinition)
                  case other =>
                    sys.error(s"bad database: unsupported name $other")
                }.toSeq
                Some(s.Synthetic(Some(spos), ssyntax, snames))
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
            case d.ResolvedName(dPosition(spos), ssym, sisDefinition) =>
              s.ResolvedName(Some(spos), ssym.syntax, sisDefinition)
            case other => sys.error(s"bad database: unsupported name $other")
          }
          val smessages = dmessages.map {
            case d.Message(dPosition(spos), dSeverity(ssym), smessage) =>
              s.Message(Some(spos), ssym, smessage)
            case other => sys.error(s"bad database: unsupported message $other")
          }
          val ssymbols = dsymbols.map {
            case d.ResolvedSymbol(ssym, dDenotation(sdefn)) =>
              s.ResolvedSymbol(ssym.syntax, Some(sdefn))
            case other => sys.error(s"bad database: unsupported denotation $other")
          }
          val ssynthetics = dsynthetics.toIterator.map {
            case dSynthetic(ssynthetic) => ssynthetic
            case other => sys.error(s"bad database: unsupported synthetic $other")
          }.toSeq
          s.Document(spath, scontents, slanguage, snames, smessages, ssymbols, ssynthetics)
      }
      s.Database(sentries)
    }
  }

}
