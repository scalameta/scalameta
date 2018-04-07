package org.langmeta.tests.io

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.langmeta.internal.io.PlatformFileIO
import org.langmeta.io.AbsolutePath
import org.scalatest.FunSuite

class ZipSuite extends FunSuite {

  test("create zip file with space in the path") {
    val tmp = Files.createTempDirectory("scalameta")
    val zip = AbsolutePath(tmp).resolve("a b.zip")
    val relpath = "A.txt"
    val contents = "a\n"

    PlatformFileIO.withJarFileSystem(zip, create = true) { root =>
      Files.write(root.resolve(relpath).toNIO, contents.getBytes(StandardCharsets.UTF_8))
    }

    PlatformFileIO.withJarFileSystem(
      zip,
      create = true
    ) { root =>
      val obtained = new String(root.resolve(relpath).readAllBytes, StandardCharsets.UTF_8)
      assert(obtained == contents)
    }
  }

}
