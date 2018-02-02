package scala.meta.tests.semanticdb

import scala.meta.internal.semanticdb.scalac.MemberMode
import scala.meta.internal.semanticdb.scalac.OverrideMode
import scala.meta.internal.semanticdb.scalac.SemanticdbMode
import scala.meta.testkit.DiffAssertions
import org.langmeta.internal.io.InputStreamIO
import org.langmeta.semanticdb.Database
import org.scalatest.FunSuite

class LegacySemanticdbSuite extends FunSuite with DiffAssertions {
  test("Database.load(Legacy)") {
    val bytes =
      InputStreamIO.readBytes(this.getClass.getClassLoader.getResourceAsStream("Legacy.semanticdb"))
    val obtained = Database.load(bytes).syntax
    assertNoDiff(
      obtained,
      """
        |src/main/scala/example/User.scala
        |---------------------------------
        |Language:
        |Scala212
        |
        |Names:
        |[8..9): a <= _root_.a.
        |[22..26): User <= _root_.a.User#
        |[26..26): ε <= _root_.a.User#`<init>`(Ljava/lang/String;I)V.
        |[27..31): name <= _root_.a.User#(name)
        |[33..39): String => _root_.scala.Predef.String#
        |[41..44): age <= _root_.a.User#(age)
        |[46..49): Int => _root_.scala.Int#
        |[59..60): a <= _root_.a.a.
        |[69..70): x <= _root_.a.a.x.
        |[84..85): y <= _root_.a.a.y.
        |[88..92): List => _root_.scala.collection.immutable.List.
        |[96..97): x => _root_.a.a.x.
        |[99..105): length => _root_.scala.collection.LinearSeqOptimized#length()I.
        |[112..113): z <= _root_.a.a.z()I.
        |[126..137): localSymbol <= local_src_main_scala_example_User_scala_122__145
        |[168..179): localSymbol => local_src_main_scala_example_User_scala_122__145
        |[180..186): length => _root_.java.lang.String#length()I.
        |
        |Symbols:
        |_root_.a. => package a
        |_root_.a.User# => case class User
        |_root_.a.User#(age) => val age: Int
        |  [0..3): Int => _root_.scala.Int#
        |_root_.a.User#(name) => val name: String
        |  [0..6): String => _root_.scala.Predef.String#
        |_root_.a.User#`<init>`(Ljava/lang/String;I)V. => primaryctor <init>: (name: String, age: Int): User
        |  [7..13): String => _root_.scala.Predef.String#
        |  [20..23): Int => _root_.scala.Int#
        |  [26..30): User => _root_.a.User#
        |_root_.a.a. => final object a
        |_root_.a.a.x. => val x: String
        |  [0..6): String => _root_.java.lang.String#
        |_root_.a.a.y. => val y: Int
        |  [0..3): Int => _root_.scala.Int#
        |_root_.a.a.z()I. => def z: Int
        |  [0..3): Int => _root_.scala.Int#
        |_root_.java.lang.String#length()I. => def length: (): Int
        |  [4..7): Int => _root_.scala.Int#
        |_root_.scala.Int# => abstract final class Int
        |_root_.scala.Int#`<init>`()V. => primaryctor <init>: (): Int
        |  [4..7): Int => _root_.scala.Int#
        |_root_.scala.Predef.String# => type String: String
        |  [0..6): String => _root_.java.lang.String#
        |_root_.scala.collection.LinearSeqOptimized#length()I. => def length: Int
        |  [0..3): Int => _root_.scala.Int#
        |_root_.scala.collection.immutable.List. => final object List
        |local_src_main_scala_example_User_scala_122__145 => val localSymbol: String
        |  [0..6): String => _root_.java.lang.String#
        |
        |Synthetics:
        |[92..92): *.apply[Any]
        |  [0..1): * => _star_.
        |  [2..7): apply => _root_.scala.collection.immutable.List.apply(Lscala/collection/Seq;)Lscala/collection/immutable/List;.
        |  [8..11): Any => _root_.scala.Any#
      """.stripMargin
    )
  }
}
