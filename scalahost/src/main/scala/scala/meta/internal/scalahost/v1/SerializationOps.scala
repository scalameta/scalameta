package scala.meta.internal.scalahost.v1

import scala.meta.semantic.v1.Symbol
import scala.meta.semantic.v1._
import scala.util.Try

import java.io.File
import java.io.FileInputStream
import java.io.FileOutputStream

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

object ProtoEncoder {
  implicit class XtensionProtoSerializable[A](val a: A) extends AnyVal {
    def toMeta[B](implicit ev: ProtoDecoder[B, A]): B = ev.fromProto(a)
    def toProto[B](implicit ev: ProtoEncoder[A, B]): B = ev.toProto(a)
    def toProtoOpt[B](implicit ev: ProtoEncoder[A, B]): Option[B] = Option(toProto[B])
  }
  implicit val AddressProto = new ProtoCodec[Address, p.Address] {
    override def toProto(e: Address): p.Address = e match {
      case Address.File(path) => p.Address(path = path)
      case Address.Snippet(contents) => p.Address(contents = contents)
    }
    override def fromProto(e: p.Address): Address = {
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
  implicit val DatabaseProto = new ProtoCodec[Database, p.Database] {
    override def toProto(e: Database): p.Database =
      p.Database {
        e.symbols
          .groupBy(_._1.addr)
          .map {
            case (addr, symbols) =>
              p.DatabaseFile(
                address = addr.toProtoOpt[p.Address],
                symbols = symbols.map(_.toProto[p.ResolvedName]).toSeq
              )
          }
          .toSeq
      }
    override def fromProto(e: p.Database): Database =
      Database(e.files.flatMap {
        case p.DatabaseFile(Some(addr), symbols) =>
          symbols.map {
            case p.ResolvedName(Some(p.Range(start, end)), Symbol(symbol)) =>
              Location(addr.toMeta[Address], start, end) -> symbol
          }
        case _ => sys.error(s"Invalid protobuf: $e")
      }.toMap)
  }
}

object SerializationOps {
  import ProtoEncoder._
  def fromBinary(bytes: Array[Byte]): Try[Database] =
    Try(p.Database.parseFrom(bytes).toMeta[Database])
  def fromFile(file: File): Try[Database] =
    Try(p.Database.parseFrom(new FileInputStream(file)).toMeta[Database])
  def toBinary(database: Database): Array[Byte] = database.toProto[p.Database].toByteArray
  def writeDatabaseToFile(database: Database, file: File): Unit = {
    val binary = toBinary(database)
    val fos = new FileOutputStream(file)
    try fos.write(binary)
    finally { fos.close() }
  }
}
