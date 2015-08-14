package org.scalameta

package object adt {
  implicit class XtensionAdtDebug(debug: org.scalameta.debug.Debug.type) {
    def adt = sys.props("adt.debug") != null
  }
}