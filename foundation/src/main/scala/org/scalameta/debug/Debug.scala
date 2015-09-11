package org.scalameta.debug

object Debug {
  def verbose = sys.props("verbose.debug") != null
}