package org.scalameta

import scala.language.experimental.macros

package object explore {
  def wildcardImportStatics(packageName: String): List[String] = macro ExploreMacros.wildcardImportStaticsImpl

  def allStatics(packageName: String): List[String] = macro ExploreMacros.allStaticsImpl

  def allSurface(packageName: String): List[String] = macro ExploreMacros.allSurfaceImpl
}