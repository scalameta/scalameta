/*
 * Protocol Buffers - Google's data interchange format
 * Copyright 2008 Google Inc.  All rights reserved.
 *
 * Use of this source code is governed by a BSD-style
 * license that can be found in META-INF/LICENSE-protobuf.txt or at
 * https://developers.google.com/open-source/licenses/bsd
 *
 * Adapted by scalameta from WireFormat of protobuf-java: changed to the API that the SemanticDB
 * streams need.
 */
package dotty.tools.dotc.semanticdb.internal

object WireFormat {

  final val WireTypeVarint = 0
  final val WireTypeFixed64 = 1
  final val WireTypeLengthDelimited = 2
  final val WireTypeStartGroup = 3
  final val WireTypeEndGroup = 4
  final val WireTypeFixed32 = 5

  private final val TagTypeBits = 3

  def getTagWireType(tag: Int): Int = tag & (1 << TagTypeBits) - 1
  def getTagFieldNumber(tag: Int): Int = tag >>> TagTypeBits
  def makeTag(fieldNumber: Int, wireType: Int): Int = fieldNumber << TagTypeBits | wireType

}
