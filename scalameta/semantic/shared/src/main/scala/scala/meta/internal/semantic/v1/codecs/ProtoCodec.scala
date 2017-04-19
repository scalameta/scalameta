package scala.meta.internal.semantic.v1.codecs

/** Evidence that A can be serialized to a protobuf message and vice-versa.
  *
  * This typeclass is split into encode/decode because the binary protocol mapping
  * is not bijective with the Database encoding. For example, there is no
  * ResolvedNameDecoder.
  */
trait ProtoCodec[A, B] extends ProtoDecoder[A, B] with ProtoEncoder[A, B]

trait ProtoEncoder[A, B] {
  def toProto(e: A): B
}

trait ProtoDecoder[A, B] {
  def fromProto(e: B): A
}
