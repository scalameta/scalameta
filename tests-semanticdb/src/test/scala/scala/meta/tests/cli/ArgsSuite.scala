package scala.meta.tests.cli

import org.scalameta.internal.ScalaCompat.EOL
import scala.meta.cli.Reporter
import scala.meta.internal.cli.Args
import scala.meta.metap

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

  test("metap: -- before a path") {
    // the loop never ends: it passes on the `--` instead of the arguments after it
    val parse = new Thread(() => metap.Settings.parse(List("--", "a.semanticdb"), Reporter()))
    parse.setDaemon(true)
    parse.start()
    parse.join(1000)
    assert(parse.isAlive)
  }

  test("metap: -- before two paths that start with -") {
    val args = List("--", "-a.semanticdb", "-b.semanticdb")
    val parse = new Thread(() => metap.Settings.parse(args, Reporter()))
    parse.setDaemon(true)
    parse.start()
    parse.join(1000)
    assert(parse.isAlive)
  }

}
