package scala.meta.tests.semanticdb

import scala.meta.internal.metap.PrinterSymtab
import scala.meta.internal.scalacp.SymlinkChildren
import scala.meta.internal.semanticdb.Print
import scala.meta.internal.semanticdb.scalac.SemanticdbOps
import scala.meta.internal.{semanticdb => s}
import scala.meta.metap.Format

class SignatureSuite extends SemanticdbSuite {

  private lazy val ops: SemanticdbOps { val global: SignatureSuite.this.g.type } = new SemanticdbOps {
    val global: SignatureSuite.this.g.type = SignatureSuite.this.g
  }
  import ops._

  /** the information of `gsym` as metap prints it, also for a Java symbol */
  private def info(gsym: g.Symbol): String = Print.info(
    Format.Detailed,
    gsym.toSymbolInformation(SymlinkChildren),
    PrinterSymtab.fromTextDocument(s.TextDocument()),
  )

  /** compiles `code`, then returns `fn` applied to the module class of its package `j` */
  private def javaPackage[A](code: String)(fn: g.Symbol => A): A = {
    val run = new g.Run
    run.compileSources(List(g.newSourceFile(code, "J.java")))
    g.exitingTyper(fn(g.rootMirror.getRequiredPackage("j").moduleClass))
  }

  symbols(
    """|object a {
       |  def m(): Unit = {
       |    class c { self => }
       |    ()
       |  }
       |}
       |""".stripMargin,
    """|_empty_/a. => final object a extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |_empty_/a.m(). => method m(): Unit
       |  Unit => scala/Unit#
       |local0 => class c extends AnyRef { +1 decls }
       |  AnyRef => scala/AnyRef#
       |local1 => primary ctor <init>()
       |local2 => selfparam self
       |""".stripMargin,
  )

  test("Java type parameters") {
    val code =
      """|package j;
         |public class J<A, B extends Comparable<B>, C extends Number & Comparable<C>> {
         |  public <D extends Runnable & AutoCloseable> void m() {}
         |}
         |""".stripMargin
    val (a, obtained) = javaPackage(code) { pkg =>
      val cls = pkg.info.decl(g.TypeName("J"))
      val tparams = cls.typeParams ++ cls.info.decl(g.TermName("m")).info.typeParams
      (tparams.head.toSymbolInformation(SymlinkChildren).signature, tparams.map(info))
    }
    val obj = s.TypeRef(s.NoType, "java/lang/Object#", Nil)
    assertEquals(a, s.TypeSignature(Some(s.Scope()), s.NoType, obj))
    assertNoDiff(
      obtained.mkString("\n"),
      """|j/J#[A] => typeparam A
         |j/J#[B] => typeparam B <: Comparable[B]
         |  Comparable => java/lang/Comparable#
         |  B => j/J#[B]
         |j/J#[C] => typeparam C <: Number & Comparable[C]
         |  Number => java/lang/Number#
         |  Comparable => java/lang/Comparable#
         |  C => j/J#[C]
         |j/J#m().[D] => typeparam D <: Runnable & AutoCloseable
         |  Runnable => java/lang/Runnable#
         |  AutoCloseable => java/lang/AutoCloseable#
         |""".stripMargin,
    )
  }

  test("Java annotations") {
    val code =
      """|package j;
         |public @interface K {}
         |""".stripMargin
    val k = javaPackage(code)(pkg => info(pkg.info.decl(g.TypeName("K"))))
    val deprecated = g
      .exitingTyper(info(g.rootMirror.getRequiredClass("java.lang.Deprecated").initialize))
    assertNoDiff(
      k + "\n" + deprecated,
      """|j/K# => abstract interface K extends Object with Annotation
         |  Object => java/lang/Object#
         |  Annotation => java/lang/annotation/Annotation#
         |java/lang/Deprecated# => @Documented @Retention @Target abstract interface Deprecated extends Object with Annotation { +2 decls }
         |  Documented => java/lang/annotation/Documented#
         |  Retention => java/lang/annotation/Retention#
         |  Target => java/lang/annotation/Target#
         |  Object => java/lang/Object#
         |  Annotation => java/lang/annotation/Annotation#
         |""".stripMargin,
    )
  }

}
