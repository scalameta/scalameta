/*
 * Protocol Buffers - Google's data interchange format
 * Copyright 2008 Google Inc.  All rights reserved.
 *
 * Use of this source code is governed by a BSD-style
 * license that can be found in META-INF/LICENSE-protobuf.txt or at
 * https://developers.google.com/open-source/licenses/bsd
 *
 * Adapted by scalameta from InvalidProtocolBufferException of protobuf-java: changed to the
 * errors that SemanticdbInputStream throws.
 */
package dotty.tools.dotc.semanticdb.internal

import java.io.IOException

final class InvalidProtocolBufferException(description: String) extends IOException(description)

object InvalidProtocolBufferException {

  def truncatedMessage(): InvalidProtocolBufferException = new InvalidProtocolBufferException(
    "While parsing a protocol message, the input ended unexpectedly in the middle of a field.  " +
      "This could mean either that the input has been truncated or that an embedded message " +
      "misreported its own length.",
  )

  def negativeSize(): InvalidProtocolBufferException = new InvalidProtocolBufferException(
    "SemanticdbInputStream encountered an embedded string or message which claimed to have " +
      "negative size.",
  )

  def malformedVarint(): InvalidProtocolBufferException =
    new InvalidProtocolBufferException("SemanticdbInputStream encountered a malformed varint.")

  def invalidTag(): InvalidProtocolBufferException =
    new InvalidProtocolBufferException("Protocol message contained an invalid tag (zero).")

  def invalidEndTag(): InvalidProtocolBufferException =
    new InvalidProtocolBufferException("Protocol message end-group tag did not match expected tag.")

  def invalidWireType(): InvalidProtocolBufferException =
    new InvalidProtocolBufferException("Protocol message tag had invalid wire type.")

  def recursionLimitExceeded(): InvalidProtocolBufferException = new InvalidProtocolBufferException(
    "Protocol message had too many levels of nesting.  May be malicious.",
  )

  def invalidUtf8(): InvalidProtocolBufferException =
    new InvalidProtocolBufferException("Protocol message had invalid UTF-8.")

}
