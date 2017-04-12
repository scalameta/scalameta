package scala.meta.internal.scalahost.v1

import scala.meta.io.AbsolutePath
import scala.meta.semantic.v1.Symbol
import scala.meta.semantic.v1._

import org.scalameta.semantic.v1.{proto => p}

/** Evidence that A can be serialized to a protobuf message and vice-versa.
  *
  * This typeclass is split into encode/decode because the binary protocol mapping
  * is not bijective with the Database encoding. For example, there is no
  * ResolvedNameDecoder.
  */
trait ProtoCodec[A, B] extends ProtoDecoder[A, B] with ProtoEncoder[A, B]
trait ProtoEncoder[A, B] { def toProto(e: A): B }
trait ProtoDecoder[A, B] { def fromProto(e: B): A }

object ProtoCodec {
  implicit class XtensionProtoSerializable[A](val a: A) extends AnyVal {
    def toMeta[B](implicit ev: ProtoDecoder[B, A]): B = ev.fromProto(a)
    def toProto[B](implicit ev: ProtoEncoder[A, B]): B = ev.toProto(a)
    def toProtoOpt[B](implicit ev: ProtoEncoder[A, B]): Option[B] = Option(toProto[B])
  }

  private def fail(e: Any): Nothing = sys.error(s"Invalid protobuf! $e")

  implicit val AddressProto = new ProtoCodec[Address, p.Address] {
    override def toProto(e: Address): p.Address = e match {
      case Address.File(AbsolutePath(path)) => p.Address(path = path)
      case Address.Snippet(code) => p.Address(contents = code)
    }

    def fromProto(e: p.Address): Address = {
      if (e.path.isEmpty) Address.Snippet(e.contents)
      else Address.File(e.path)
    }
  }
  implicit val ResolvedNameProto = new ProtoEncoder[(Location, Symbol), p.ResolvedName] {
    override def toProto(e: (Location, Symbol)): p.ResolvedName = e match {
      case (Location(_, start, end), sym) =>
        p.ResolvedName(Option(p.Range(start, end)), sym.syntax)
    }
  }
  implicit val CompilerMessageProto = new ProtoEncoder[CompilerMessage, p.CompilerMessage] {
    override def toProto(e: CompilerMessage): p.CompilerMessage = e match {
      case CompilerMessage(Location(addr, start, end), sev, msg) =>
        p.CompilerMessage(Option(p.Range(start, end)),
                          p.CompilerMessage.Severity.fromValue(sev.id),
                          msg)
    }
  }
  implicit val SymbolDenotationProto = new ProtoEncoder[(Symbol, Denotation), p.SymbolDenotation] {
    override def toProto(e: (Symbol, Denotation)): p.SymbolDenotation = e match {
      case (sym, denot) =>
        p.SymbolDenotation(sym.syntax, Option(p.Denotation(denot.flags)))
    }
  }
  implicit val DatabaseProto = new ProtoCodec[Database, p.Database] {
    override def toProto(e: Database): p.Database = {
      val messagesGrouped = e.messages.groupBy(_.location.addr).withDefaultValue(Nil)
      val files = e.names
        .groupBy(_._1.addr)
        .map {
          case (addr, names) =>
            val messages = messagesGrouped(addr).map(_.toProto[p.CompilerMessage])
            p.DatabaseFile(
              address = addr.toProtoOpt[p.Address],
              names = names.map(_.toProto[p.ResolvedName]).toSeq,
              messages = messages,
              denotations = e.denotations.map(_.toProto[p.SymbolDenotation]).toSeq
            )
        }
        .toSeq
      p.Database(files)
    }
    override def fromProto(e: p.Database): Database = {
      val names = e.files.flatMap {
        case p.DatabaseFile(Some(addr), names, _, _) =>
          names.map {
            case p.ResolvedName(Some(p.Range(start, end)), Symbol(symbol)) =>
              Location(addr.toMeta[Address], start, end) -> symbol
          }
        case _ => fail(e)
      }.toMap
      val messages: Seq[CompilerMessage] = e.files.flatMap {
        case p.DatabaseFile(Some(addr), l, messages, _) =>
          messages.map {
            case p.CompilerMessage(Some(p.Range(start, end)), sev, message) =>
              CompilerMessage(Location(addr.toMeta[Address], start, end),
                              Severity.fromId(sev.value),
                              message)
            case e => fail(e)
          }
        case e => fail(e)
      }
      val denotations = e.files.flatMap {
        case p.DatabaseFile(_, _, _, denotations) =>
          denotations.map {
            case p.SymbolDenotation(Symbol(symbol), Some(p.Denotation(flags))) =>
              symbol -> Denotation(flags)
          }
        case _ => fail(e)
      }.toMap
      Database(names, messages, denotations)
    }
  }
}
