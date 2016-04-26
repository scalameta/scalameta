package org.scalameta

import scala.language.experimental.macros

package object explore {
  def wildcardImportToplevels(packageName: String): List[String] = macro ExploreMacros.wildcardImportToplevels

  def wildcardImportExtensions(packageName: String): List[String] = macro ExploreMacros.wildcardImportExtensions

  def entireSurface(packageName: String): List[String] = macro ExploreMacros.entireSurface
}