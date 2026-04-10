package org.scalameta.internal

object AdtHelpers {

  def getterName(name: String): String = name.stripPrefix("_")
  def privateName(name: String): String = "_" + getterName(name)
  def setterName(name: String): String = "set" + getterName(name).capitalize

}
