package scala.meta
package tests

import org.scalatest._

class CrossPlatformSemanticSuite extends FunSuite {
  test("Database.load(Array[Byte])") {
    Database.load(RelativePath("Foo.scala"))
  }
}