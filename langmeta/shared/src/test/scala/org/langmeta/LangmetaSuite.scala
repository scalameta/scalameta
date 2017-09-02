package org.langmeta

import org.langmeta.internal.io.FileIO
import org.langmeta.internal.io.PathIO

class LangmetaSuite extends org.scalatest.FunSuite {
  test("everything compiles OK") {
    Symbol("_root_.a.")
    FileIO.listAllFilesRecursively(PathIO.workingDirectory)
  }
}
