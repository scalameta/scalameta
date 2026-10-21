package scala.meta.tests.cli

import org.scalameta.internal.ScalaCompat.EOL
import scala.meta.internal.cli.Args

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Files

class ArgsSuite extends munit.FunSuite {

  test("an argument and an @file of arguments") {
    val file = Files.createTempFile("args", ".txt")
    try {
      Files.write(file, List(" -a ", "", "b").mkString(EOL).getBytes(UTF_8))
      assertEquals(Args.expand(Array("x", s"@$file")), List("x", "-a", "b"))
    } finally Files.delete(file)
  }

}
