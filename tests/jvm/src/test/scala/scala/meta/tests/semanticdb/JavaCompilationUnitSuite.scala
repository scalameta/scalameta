package scala.meta.tests.semanticdb

class JavaCompilationUnitSuite extends SemanticdbSuite {
  override def trimLines: Boolean = false

  javaSymbols(
    """package ja;
      |public class A {
      |  public A(int a) { }
      |  public int field;
      |  public int method();
      |  public static int staticField;
      |  public static int staticMethod();
      |}
    """.stripMargin,
    """|ja.A# => class A extends Object { +5 decls }
       |  Object => java.lang.Object#
       |ja.A#`<init>`(). => ctor <init>(a: Int)
       |  a => ja.A#`<init>`().(a)
       |  Int => scala.Int#
       |ja.A#`<init>`().(a) => param a: Int
       |  Int => scala.Int#
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

  javaSymbols(
    """package jc;
      |public class Overload {
      |  private final int value;
      |  public int value() { return value; }
      |}
    """.stripMargin,
    """|jc.Overload# => class Overload extends Object { +3 decls }
       |  Object => java.lang.Object#
       |jc.Overload#`<init>`(). => ctor <init>()
       |jc.Overload#value(). => method value(): Int
       |  Int => scala.Int#
       |jc.Overload#value. => private field value: Int
       |  Int => scala.Int#
    """.stripMargin
  )
  javaSymbols(
    """package jd;
      |public class Decls {
      |    class A {
      |    }
      |}
    """.stripMargin,
    """|jd.Decls# => class Decls extends Object { +2 decls }
       |  Object => java.lang.Object#
       |jd.Decls#A# => private[jd] class A extends Object { +1 decls }
       |  jd => jd.
       |  Object => java.lang.Object#
       |jd.Decls#A#`<init>`(). => ctor <init>()
       |jd.Decls#`<init>`(). => ctor <init>()
    """.stripMargin
  )

  javaSymbols(
    """package je;
      |class Inner {
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
    """|je.Inner# => private[je] class Inner extends Object { +4 decls }
       |  je => je.
       |  Object => java.lang.Object#
       |je.Inner#B# => class B extends Object { +2 decls }
       |  Object => java.lang.Object#
       |je.Inner#B#`<init>`(). => ctor <init>()
       |je.Inner#B#b(). => method b(): Int
       |  Int => scala.Int#
       |je.Inner#C# => static class C extends Object { +3 decls }
       |  Object => java.lang.Object#
       |je.Inner#C#`<init>`(). => ctor <init>()
       |je.Inner#C#c(). => static method c(): Int
       |  Int => scala.Int#
       |je.Inner#C#cc(). => method cc(): Int
       |  Int => scala.Int#
       |je.Inner#D# => abstract interface D extends Object { +1 decls }
       |  Object => java.lang.Object#
       |je.Inner#D#d(). => abstract method d(): Int
       |  Int => scala.Int#
       |je.Inner#`<init>`(). => ctor <init>()
    """.stripMargin
  )
}
