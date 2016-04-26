package org.scalameta

import scala.language.experimental.macros

package object explore {
  def publicToplevelDefinitions(packageName: String): List[String] = macro ExploreMacros.publicToplevelDefinitions

  def publicToplevelExtensionMethods(packageName: String): List[String] = macro ExploreMacros.publicToplevelExtensionMethods

  def publicSurface(packageName: String): List[String] = macro ExploreMacros.publicSurface
}