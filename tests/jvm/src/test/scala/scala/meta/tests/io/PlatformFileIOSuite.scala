package scala.meta.tests
package io

import scala.meta._
import scala.meta.internal.io._

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{FileSystems, Files}

import munit.FunSuite

class PlatformFileIOSuite extends FunSuite {
  // #1168: an unreadable subdirectory must be skipped, not abort the whole traversal
  test("listAllFilesRecursively skips unreadable directories") {
    assume(
      FileSystems.getDefault.supportedFileAttributeViews.contains("posix"),
      "POSIX file permissions not supported on this platform",
    )
    val root = AbsolutePath(Files.createTempDirectory("scalameta-1168"))
    val ok = root.resolve("ok.txt")
    Files.write(ok.toNIO, "ok".getBytes)
    val secret = root.resolve("secret")
    Files.createDirectory(secret.toNIO)
    Files.setPosixFilePermissions(secret.toNIO, PosixFilePermissions.fromString("---------"))
    // running as root bypasses permission checks, so the directory stays readable: skip then
    assume(!Files.isReadable(secret.toNIO), "directory still readable (running as root?)")
    try {
      val obtained = FileIO.listAllFilesRecursively(root) // must not throw
      assert(obtained.contains(ok), s"$ok missing from:\n${obtained.mkString("\n")}")
    } finally {
      // restore permissions and delete the temporary tree
      Files.setPosixFilePermissions(secret.toNIO, PosixFilePermissions.fromString("rwx------"))
      Files.delete(ok.toNIO)
      Files.delete(secret.toNIO)
      Files.delete(root.toNIO)
    }
  }
}
