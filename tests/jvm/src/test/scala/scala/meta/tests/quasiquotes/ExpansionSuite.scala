package scala.meta.tests
package quasiquotes

import scala.meta._
import scala.meta.dialects.Scala213

import java.io.DataInputStream

import munit._

// a quasiquote expands into the client's bytecode, so the names it calls must outlive a release
object ExpansionSuite {
  object TermExpansion {
    val tree: Tree = q"if (a) b else c"
  }

  object PatternExpansion {
    def matches(tree: Tree): Boolean = tree match {
      case q"if ($x) $y else $z" => true
      case _ => false
    }
  }
}

class ExpansionSuite extends FunSuite {
  import ExpansionSuite._

  // the classes a class file refers to, and the methods it calls as `owner.name`
  private def refs(name: String): (Set[String], Set[String]) = {
    val in = new DataInputStream(getClass.getResourceAsStream("/" + name + ".class"))
    in.readInt()
    in.readUnsignedShort()
    in.readUnsignedShort()
    val count = in.readUnsignedShort()
    val utf8 = new Array[String](count)
    val cls = new Array[Int](count)
    val nameOf = new Array[Int](count)
    val calls = List.newBuilder[(Int, Int)]
    var i = 1
    while (i < count) {
      in.readUnsignedByte() match {
        case 1 => utf8(i) = in.readUTF()
        case 3 | 4 => in.readInt()
        case 5 | 6 =>
          in.readLong()
          i += 1
        case 7 => cls(i) = in.readUnsignedShort()
        case 8 | 16 | 19 | 20 => in.readUnsignedShort()
        case 9 | 11 => in.readInt()
        case 10 => calls += ((in.readUnsignedShort(), in.readUnsignedShort()))
        case 12 =>
          nameOf(i) = in.readUnsignedShort()
          in.readUnsignedShort()
        case 15 =>
          in.readUnsignedByte()
          in.readUnsignedShort()
        case 17 | 18 => in.readInt()
      }
      i += 1
    }
    in.close()
    val classes = cls.iterator.filter(_ != 0).map(utf8).toSet
    (
      classes,
      calls.result().iterator.map { case (c, nat) => utf8(cls(c)) + "." + utf8(nameOf(nat)) }.toSet,
    )
  }

  /* what the object's class file and the ones it spawns call:
   * on Scala 2 the pattern's `unapply` compiles into an anonymous class */
  private def expansion(obj: AnyRef): Set[String] = {
    val own = obj.getClass.getName.replace('.', '/')
    def walk(name: String, seen: Set[String]): (Set[String], Set[String]) = {
      val (classes, calls) = refs(name)
      classes.filter(_.startsWith(own)).diff(seen).foldLeft((seen + name, calls)) {
        case ((seen, calls), next) =>
          val (s, c) = walk(next, seen)
          (s, calls ++ c)
      }
    }
    walk(own, Set.empty)._2
  }

  private val term = expansion(TermExpansion)
  private val pattern = expansion(PatternExpansion)

  private def about(calls: Set[String]) = calls.filter(_.startsWith("scala/meta/Term$If"))

  test("a term")(assertEquals(
    about(term),
    Set(
      "scala/meta/Term$If$.newBuilder",
      "scala/meta/Term$If$Builder$.mods$extension",
      "scala/meta/Term$If$Builder$.origin$extension",
      "scala/meta/Term$If$Builder$.begComment$extension",
      "scala/meta/Term$If$Builder$.endComment$extension",
      "scala/meta/Term$If$Builder$.result$extension",
    ),
  ))

  test("a pattern")(assertEquals(about(pattern), Set("scala/meta/Term$If$After_4_4_0$.unapply")))

  test("never the alias that moves")(
    assertEquals((term ++ pattern).filter(_.contains("Latest")), Set.empty[String]),
  )

}
