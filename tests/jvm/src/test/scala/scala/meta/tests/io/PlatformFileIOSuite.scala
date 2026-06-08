package scala.meta.tests
package io

import scala.meta._
import scala.meta.internal.io._

import java.nio.file.attribute.PosixFilePermissions
import java.nio.file.{FileSystems, Files}

import munit.FunSuite

class PlatformFileIOSuite extends FunSuite {
  // #1168: listAllFilesRecursively aborts the walk when it meets an unreadable directory
  test("listAllFilesRecursively aborts on unreadable directories") {
    assume(
      FileSystems.getDefault.supportedFileAttributeViews.contains("posix"),
      "POSIX file permissions not supported on this platform",
    )
    val root = AbsolutePath(Files.createTempDirectory("scalameta-1168"))
    val secret = root.resolve("secret")
    Files.createDirectory(secret.toNIO)
    Files.setPosixFilePermissions(secret.toNIO, PosixFilePermissions.fromString("---------"))
    // running as root bypasses permission checks, so the directory stays readable: skip then
    assume(!Files.isReadable(secret.toNIO), "directory still readable (running as root?)")
    try intercept[java.io.UncheckedIOException](FileIO.listAllFilesRecursively(root))
    finally {
      // restore permissions and delete the temporary tree
      Files.setPosixFilePermissions(secret.toNIO, PosixFilePermissions.fromString("rwx------"))
      Files.delete(secret.toNIO)
      Files.delete(root.toNIO)
    }
  }
}
