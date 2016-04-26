package org.scalameta

import scala.language.experimental.macros

package object explore {
  def publicToplevelDefinitions(packageName: String): List[String] = macro ExploreMacros.publicToplevelDefinitions

  def publicExtensionMethods(packageName: String): List[String] = macro ExploreMacros.publicExtensionMethods

  def publicSurface(packageName: String): List[String] = macro ExploreMacros.publicSurface
}