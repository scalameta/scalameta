package scala.meta.tests.semanticdb

class JavaCompilationUnitSuite extends SemanticdbSuite {
  override def trimLines: Boolean = false

  javaSymbols(
    """package ja;
      |public class A {
      |  public int field;
      |  public int method();
      |  public static int staticField;
      |  public static int staticMethod();
      |  public class B {
      |    public int b() { return 1; }
      |  }
      |  public static class C {
      |    public static int c() { return 1; }
      |    public int cc() { return 1; }
      |  }
      |  public interface D {
      |    public int d();
      |  }
      |}
    """.stripMargin,
    """|ja.A# => class A extends Object { +11 decls }
       |  Object => java.lang.Object#
       |ja.A#B# => class B extends Object { +2 decls }
       |  Object => java.lang.Object#
       |ja.A#B#`<init>`(). => ctor <init>()
       |ja.A#B#b(). => method b(): Int
       |  Int => scala.Int#
       |ja.A#C# => static class C extends Object { +3 decls }
       |  Object => java.lang.Object#
       |ja.A#C#`<init>`(). => ctor <init>()
       |ja.A#C#c(). => static method c(): Int
       |  Int => scala.Int#
       |ja.A#C#cc(). => method cc(): Int
       |  Int => scala.Int#
       |ja.A#D# => abstract interface D extends Object { +1 decls }
       |  Object => java.lang.Object#
       |ja.A#D#d(). => abstract method d(): Int
       |  Int => scala.Int#
       |ja.A#`<init>`(). => ctor <init>()
       |ja.A#field. => field field: Int
       |  Int => scala.Int#
       |ja.A#method(). => method method(): Int
       |  Int => scala.Int#
       |ja.A#staticField. => static field staticField: Int
       |  Int => scala.Int#
       |ja.A#staticMethod(). => static method staticMethod(): Int
       |  Int => scala.Int#
    """.stripMargin
  )

  javaSymbols(
    """package jb;
      |public interface A {
      |  void a();
      |  static void b() { }
      |}
    """.stripMargin,
    """|jb.A# => abstract interface A extends Object { +2 decls }
       |  Object => java.lang.Object#
       |jb.A#a(). => abstract method a(): Unit
       |  Unit => scala.Unit#
       |jb.A#b(). => static method b(): Unit
       |  Unit => scala.Unit#
    """.stripMargin
  )

}
