package scala.meta.tests
package transversers

import org.scalatest._
import scala.compat.Platform.EOL
import scala.meta._
import scala.meta.internal.semantic._
import scala.meta.internal.prettyprinters._
import scala.meta.internal.ast.Origin

class TransverserSuite extends FunSuite {
  test("Traverser Ok") {
    val tree0 = q"""
      def foo(x: x)(x: Int) = x + x
      class C(x: x) {
        def bar(x: x) = ???
      }
    """
    val log = scala.collection.mutable.ListBuffer[String]()
    object traverser extends Traverser {
      override def apply(tree: Tree): Unit = {
        log += tree.toString.trim.replace("\n", " ")
        super.apply(tree)
      }
    }
    traverser(tree0)
    assert(log.mkString(EOL) === """
      |{   def foo(x: x)(x: Int) = x + x   class C(x: x) { def bar(x: x) = ??? } }
      |def foo(x: x)(x: Int) = x + x
      |foo
      |x: x
      |x
      |x
      |x: Int
      |x
      |Int
      |x + x
      |x
      |+
      |x
      |class C(x: x) { def bar(x: x) = ??? }
      |C
      |def this(x: x)
      |this
      |x: x
      |x
      |x
      |{ def bar(x: x) = ??? }
      |_
      |_
      |def bar(x: x) = ???
      |bar
      |x: x
      |x
      |x
      |???
    """.trim.stripMargin)
  }

  test("Transformer Ok") {
    val tree0 = q"""
      def foo(x: x)(x: Int) = x + x
      class C(x: x) {
        def bar(x: x) = ???
      }
    """
    val log = scala.collection.mutable.ListBuffer[String]()
    object transformer extends Transformer {
      override def apply(tree: Tree): Tree = tree match {
        case Term.Name("x") => Term.Name("y")
        case Type.Name("x") => Type.Name("y")
        case _ => super.apply(tree)
      }
    }
    val tree1 = transformer(tree0).withOrigin(Origin.None)
    assert(tree1.toString === """
      |{
      |  def foo(y: y)(y: Int) = y + y
      |  class C(y: y) { def bar(y: y) = ??? }
      |}
    """.trim.stripMargin)
  }

  test("Transformer Fail") {
    val tree0 = q"""
      def foo(x: x)(x: Int) = x + x
      class C(x: x) {
        def bar(x: x) = ???
      }
    """
    val log = scala.collection.mutable.ListBuffer[String]()
    object transformer extends Transformer {
      override def apply(tree: Tree): Tree = {
        if (tree.toString == "x") q"y"
        else super.apply(tree)
      }
    }
    intercept[UnsupportedOperationException]{ transformer(tree0) }
  }

  test("Transformed Attributes") {
    def attributeTypeName(name: Type.Name): Type.Name = name.withAttrs(Denotation.Single(Prefix.None, Symbol.RootPackage))
    val Foo = attributeTypeName(Type.Name("Foo"))
    def attributeTermName(name: Term.Name): Term.Name = name.withAttrs(Denotation.Single(Prefix.None, Symbol.RootPackage), Foo.setTypechecked)
    def attributeTerm(term: Term): Term = term.withAttrs(Foo.setTypechecked)
    val denot1 = Denotation.Single(Prefix.None, Symbol.RootPackage)
    val denot2 = Denotation.Single(Prefix.None, Symbol.EmptyPackage)
    val typing = Foo.setTypechecked
    val x = q"x".withAttrs(denot1, typing).setTypechecked
    val z = q"z".withAttrs(denot2, typing).setTypechecked
    val attr0 = q"$x + $z".withAttrs(typing)
    assert(attr0.show[Attributes] === """
      |Term.ApplyInfix(Term.Name("x")[1]{1}, Term.Name("+")*, Nil, Seq(Term.Name("z")[2]{1})){1}*
      |[1] {0}::_root_
      |[2] {0}::_empty_
      |{1} Type.Name("Foo")[1]
    """.trim.stripMargin)

    object transformer extends Transformer {
      override def apply(tree: Tree): Tree = tree match {
        case Term.Name("x") => Term.Name("y")
        case Type.Name("x") => Type.Name("y")
        case _ => super.apply(tree)
      }
    }

    val attr1 = transformer(attr0)
    assert(attr1.show[Attributes] === """
      |Term.ApplyInfix(Term.Name("y")*, Term.Name("+")*, Nil, Seq(Term.Name("z")[1]{1}))*
      |[1] {0}::_empty_
      |[2] {0}::_root_
      |{1} Type.Name("Foo")[2]
    """.trim.stripMargin)
  }

  test("Tree.transform") {
    val tree0 = "x + y".parse[Term].get
    val tree1 = tree0.transform { case Term.Name(s) => Term.Name(s + s) }
    assert(tree1.toString == "xx ++ yy")
  }

  test("Tree.traverse") {
    var cnt = 0
    val tree0 = "x + y".parse[Term].get
    tree0.traverse { case Term.Name(s) => cnt += 1 }
    assert(cnt == 3)
  }

  test("Tree.collect") {
    val tree0 = q"x + y"
    val result1 = tree0.collect { case Term.Name(s) => s }
    assert(result1.toString == "List(x, +, y)")
  }   

  test("Origin preserving transforms") {
    val tree0 = "{ /* hello */ def foo(bar: Int) = bar }".parse[Term].get
    val result1 = tree0 transform { case q"bar" => q"baz" }
    result1.origin match {
      case Origin.Transformed(from, to) =>
        assert(tree0 eq from)
        assert(from.origin eq tree0.origin)
        assert(tree0.children.map(_.origin) == from.children.map(_.origin))
      case _ => assert(false)

    }
  }

  test("Preserve formatting basic test") {
    val tree0 = "{ /* hello */ def foo(bar: Int) = bar }".parse[Term].get
    val result1 = tree0 transform { case q"bar" => q"baz" }
    assert(result1.toString == "{ /* hello */ def foo(baz: Int) = baz }")
  }

  test("Basic transform tests") {
    val tree0 = "{ /* hello */ def foo(bar: Int) = bar }".parse[Term].get
    val result1 = tree0 transform { case q"bar" => q"bar" }
    assert(result1.toString == tree0.toString)
  }
   
  test("weirdly indented code") {
    val tree = """{
      if (true) {
           1
      } else {
        2

        }

    }""".parse[Stat].get
    val result1 = tree transform { case q"true" => q"false" }
    val s = """{
      if (false) {
           1
      } else {
        2

        }

    }"""
    assert(result1.toString == s)
  }
   
  test("simple transform with match") {
    val tree0 = """
      def foo(bar: Int) = bar match {
        case 1 => 1
        case _ => 2
        }""".parse[Stat].get
    val result1 = tree0 transform { case q"bar" => q"baz" }
    val s = """
      def foo(baz: Int) = baz match {
        case 1 => 1
        case _ => 2
        }"""
    assert(result1.toString == s)
  }
   
  test("Simple if test") {
    val tree0 = """ if (true) 1 else 2""".parse[Term].get
    val result1 = tree0 transform { case q"true" => q"false" }
    val s = """ if (false) 1 else 2"""
    assert(result1.toString == s)
  }   

  test("Weirdly indented if") {
    val tree0 = """if      (x)
        y
    else
             z
    """.parse[Term].get

    val result1 = tree0 transform { case q"y" => q"b" }
    val s = """if      (x)
        b
    else
             z
    """
    assert(result1.toString == s)
  }   

  test("Basic transform case with match") {
    val tree0 = """
      x match {
        case 1 => 2
        case 2 => 3
        case _ => 4
      }
      """.parse[Term].get
    val result1 = tree0 transform { case q"1" => q"5"}
    val s = """
      x match {
        case 5 => 2
        case 2 => 3
        case _ => 4
      }
      """
    assert(result1.toString == s)
  }
   
  test("transform body of match") {
    val tree0 = """
      x match {
         case 1     =>          2
         case 3 => 4
         case 5 => 6
      }
    """.parse[Term].get
    val result1 = tree0 transform { case q"2" => q"9" }
    val s = """
      x match {
         case 1     =>          9
         case 3 => 4
         case 5 => 6
      }
    """
    assert(result1.toString == s)
  }

  test("basic block test") {
    val tree0 = "{ /* hello */ if (x) true else false }".parse[Term].get
    val result1 = tree0 transform { case q"x" => q"y" }
    val s = "{ /* hello */ if (y) true else false }"
    assert(result1.toString == s)
  }

  test("few more block tests") {
    val tree0 = """{
      /* hello */
      a = y
      (a,          z)
    }""".parse[Term].get
    val result1 = tree0 transform { case q"a" => q"x" }
    val s = """{
      /* hello */
      x = y
      (x,          z)
    }"""
    assert(result1.toString == s)
  }

  test("nested block test") {
    val tree0 = "if (a > 10) { if (y > 10) { x } }".parse[Term].get
    val result1 = tree0 transform { case q"x" => q"a" }
    val s = "if (a > 10) { if (y > 10) { a } }"
    assert(result1.toString == s)
  }
   
  test("class test") {
    val tree0 = "class C(x: Int)".parse[Stat].get
    val result1 = tree0 transform { case t"C" => t"D" }
    val result2 = tree0 transform { case q"x" => q"y" }
    val s1 = "class D(x: Int)"
    val s2 = "class C(y: Int)"
    assert(s1 == result1.toString)
    assert(s2 == result2.toString) 
  }

  test("function test") {
    val tree0 = "(x: Int) => x + 1".parse[Term].get
    val result1 = tree0 transform  { case q"x" => q"y" }
    val result2 = tree0 transform { case t"Int" => t"String" }
    val s1 = "(y: Int) => y + 1"
    val s2 = "(x: String) => x + 1"
    assert(s1 == result1.toString)
    assert(s2 == result2.toString)
  }

  test("Literal test") {
    val tree0 = "1".parse[Term].get
    val result1 = tree0 transform { case q"1" => q"a" }
    val result2 = tree0 transform { case q"1" => q"()" }
    val s1 = "a"
    val s2 = "()"
    assert(s1 == result1.toString)
    assert(s2 == result2.toString)
  }

  test("function application") {
    val tree0 = "f(x, y, z)".parse[Term].get
    val result1 = tree0 transform { case q"x" => q"a" }
    val result2 = tree0 transform { case q"f" => q"g" }
    val s1 = "f(a, y, z)"
    val s2 = "g(x, y, z)"
    assert(s1 == result1.toString)
    assert(s2 == result2.toString)
  }

  test("infix application") {
    val tree0 = "foo map println".parse[Term].get
    val result1 = tree0 transform { case q"map" => q"foreach" }
    val result2 = tree0 transform { case q"foo" => q"bar" }
    val s1 = "foo foreach println"
    val s2 = "bar map println"
    assert(s1 == result1.toString)
    assert(s2 == result2.toString)
  }

  test("assign test") {
    val tree0 = "x = y".parse[Term].get
    val result1 = tree0 transform { case q"x" => q"a" }
    val s1 = "a = y"
    assert(s1 == result1.toString)
  }

  test("return test") {
    val tree0 = "return x".parse[Term].get
    val result1 = tree0 transform { case q"x" => q"a" }
    val s1 = "return a"
    assert(s1 == result1.toString)
  }

  test("throw test") {
    val tree0 = "throw x".parse[Term].get
    val result1 = tree0 transform { case q"x" => q"a" }
    val s1 = "throw a"
    assert(s1 == result1.toString)
  }

  test("ascription test") {
    val tree0 = "x: Int".parse[Term].get
    val result1 = tree0 transform { case q"x" => q"y" }
    val result2 = tree0 transform { case t"Int" => t"String" }
    val s1 = "y: Int"
    val s2 = "x: String"
    assert(s1 == result1.toString)
    assert(s2 == result2.toString)
  }

  test("tuple test") {
    val tree0 = "(x, y, z)".parse[Term].get
    val result1 = tree0 transform { case q"y" => q"b" }
    val s1 = "(x, b, z)"
    assert(s1 == result1.toString)
  }

  test("partial function test") {
    val tree0 = """
     | x map {
     |   case foo => bar
     |   case bar => baz
     | }
     | """.trim.stripMargin.parse[Term].get
    val result1 = tree0 transform { case q"bar" => q"yes" }
    val s1 = """
     | x map {
     |   case foo => yes
     |   case yes => baz
     | }
     | """.trim.stripMargin
    assert(s1 == result1.toString)
  }

  test("while test") {
    val tree0 = "while (x < 10) { println x }".parse[Term].get
    val result1 = tree0 transform { case q"x" => q"a" }
    val s1 = "while (a < 10) { println a }"
    assert(s1 == result1.toString)
  }

  test("for yield test") {
    val tree0 = """
     | for {
     |   i <- 1 to x
     |   j <- i to y
     | } yield {
     |   (i, j)
     | }
     | """.trim.stripMargin.parse[Term].get
    val result1 = tree0 transform { case q"i" => q"a" }
    val result2 = tree0 transform { case q"to" => q"until" }
    val result3 = tree0 transform { case q"x" => q"bar" }
    val s1 = """
     | for {
     |   a <- 1 to x
     |   j <- a to y
     | } yield {
     |   (a, j)
     | }
     | """.trim.stripMargin
    val s2 = """
     | for {
     |   i <- 1 until x
     |   j <- i until y
     | } yield {
     |   (i, j)
     | }
     | """.trim.stripMargin
    val s3 = """
     | for {
     |   i <- 1 to bar
     |   j <- i to y
     | } yield {
     |   (i, j)
     | }
     | """.trim.stripMargin
    assert(s1 == result1.toString)
    assert(s2 == result2.toString)
    assert(s3 == result3.toString)
  }

  // fails.
  // test("new test") {
  //   val tree0 = "new Foo { val x = 1 }".parse[Term].get
  //   val result1 = tree0 transform { case q"x" => q"y" }
  //   val result2 = tree0 transform { case q"Foo" => q"Bar" }
  //   val s1 = "new Foo { val y = 1 }"
  //   val s2 = "new Bar { val x = 1 }"

  //   assert(s1 == result1.toString)
  //   assert(s2 == result2.toString)
  // }

  test("placeholder test") {
    val tree0 = "_".parse[Term].get
    val result1 = tree0 transform { case q"_" => q"a" }
    val s1 = "a"
    assert(s1 == result1.toString)
  }

  test("eta test") {
    val tree0 = "foo _".parse[Term].get
    val result1 = tree0 transform { case q"foo _" => q"bar" }
    val s1 = "bar"
    assert(s1 == result1.toString)
  }

  test("try catch cases test") {
    val tree0 =  """
     | try {
     |   println(x)
     | } catch {
     |   case e: Exception1 => x1
     |   case e1: Exception2 => x2
     |   case e2: Exception3 => x3
     | } finally {
     |   x + y
     | }
     | """.trim.stripMargin.parse[Term].get
    val result1 = tree0 transform { case q"x" => q"y" }
    val result2 = tree0 transform { case q"e" => q"e0" }
    val result3 = tree0 transform { case t"Exception1" => t"Exception4" }
    val result4 = tree0 transform { case q"x1" => q"x3" }
    val s1 = """
     | try {
     |   println(y)
     | } catch {
     |   case e: Exception1 => x1
     |   case e1: Exception2 => x2
     |   case e2: Exception3 => x3
     | } finally {
     |   y + y
     | }
     | """.trim.stripMargin
    val s2 =  """
     | try {
     |   println(x)
     | } catch {
     |   case e0: Exception1 => x1
     |   case e1: Exception2 => x2
     |   case e2: Exception3 => x3
     | } finally {
     |   x + y
     | }
     | """.trim.stripMargin
    val s3 =  """
     | try {
     |   println(x)
     | } catch {
     |   case e: Exception4 => x1
     |   case e1: Exception2 => x2
     |   case e2: Exception3 => x3
     | } finally {
     |   x + y
     | }
     | """.trim.stripMargin
    val s4 =  """
     | try {
     |   println(x)
     | } catch {
     |   case e: Exception1 => x3
     |   case e1: Exception2 => x2
     |   case e2: Exception3 => x3
     | } finally {
     |   x + y
     | }
     | """.trim.stripMargin
    assert(s1 == result1.toString)
    assert(s2 == result2.toString)
    assert(s3 == result3.toString)
    assert(s4 == result4.toString)
  }

  test("arg test") {
    val tree0 = "def foo(bar: Int) = ???".parse[Stat].get
    val result1 = tree0 transform { case arg"bar" => arg"baz" }
    val s1 = "def foo(baz: Int) = ???"
    assert(s1 == result1.toString)
  }

  test("repeated arg") {
    val tree0 = "def foo(bar: _*) = ???".parse[Stat].get
    val result1 = tree0 transform { case arg"bar" => arg"baz" }
    val s1 = "def foo(baz: _*) = ???"
    assert(s1 == result1.toString)
  }

  test("type name") {
    val tree0 = "Int".parse[Type].get
    val result1 = tree0 transform { case t"Int" => t"String" }
    val s1 = "String"
    assert(s1 == result1.toString)
  }

  test("type selection") {
    val tree0 = "def foo(bar: X.Y) = ???".parse[Stat].get
    val result1 = tree0 transform { case t"X.Y" => t"Y.Z" }
    val s1 = "def foo(bar: Y.Z) = ???"
    assert(s1 == result1.toString)
  }

  test("type projection") {
    val tree0 = "def foo(bar: X#Y) = ???".parse[Stat].get
    val result1 = tree0 transform { case t"X#Y" => t"A#B" }
    val s1 = "def foo(bar: A#B) = ???"
    assert(s1 == result1.toString)
  }

  test("type function") {
    val tree0 = "trait Functor[F[_]] { def fmap[A, B](f: A => B)(fa: F[A]): F[B] = ??? }".parse[Stat].get
    val result1 = tree0 transform { case t"A => B" => t"A1 => B1" }
    val s1 = "trait Functor[F[_]] { def fmap[A, B](f: A1 => B1)(fa: F[A]): F[B] = ??? }"
    assert(s1 == result1.toString)
  }

  test("type tuple" ) {
    val tree0 = "def foo(bar: (A, B)) = ???".parse[Stat].get
    val result1 = tree0 transform { case t"(A, B)" => t"(B, C)" }
    val s1 = "def foo(bar: (B, C)) = ???"
    assert(s1 == result1.toString)
  }

  test("type existential") {
    val tree0 = "def foo(x: A forSome { type A }) = x".parse[Stat].get
    val result1 = tree0 transform { case t"A forSome { type A }" => t"B forSome { type B }" }
    val s1 = "def foo(x: B forSome { type B }) = x"
    assert(s1 == result1.toString)
  }

  test("import test") {
    val tree0 = "import Foo.Bar.Baz".parse[Stat].get
    val result1 = tree0 transform { case q"Foo" => q"Yes" }
    val s1 = "import Yes.Bar.Baz"
    assert(s1 == result1.toString)
  }
  
  test("infix + rename transform") {
    val tree0 = "def foo(bar: Int) = baz".parse[Stat].get
    val result1 = tree0 transform { case q"baz" => "baz + baz".parse[Term].get }
    val result2 = result1 transform { case q"baz" => "bar".parse[Term].get }
    val s1 = "def foo(bar: Int) = bar + bar"
    assert(s1 == result2.toString)
  }  
  
  test("simple change to block") {
    val tree0 = "if (x) y else z".parse[Term].get
    val result1 = tree0 transform { case q"x" => "{ a }".parse[Term].get }
    val result2 = result1 transform { case q"a" => "b".parse[Term].get }
    val s1 = "if ({ b }) y else z"
    assert(s1 == result2.toString)
  }
   
  test("basic def within a block") {
    val tree0 = "{ def foo(bar: Int) = baz }".parse[Term].get
    val result1 = tree0 transform { case q"baz" => "works".parse[Term].get }
    val s1 = "{ def foo(bar: Int) = works }"
    assert(s1 == result1.toString)
  }
   
  test("more tricky block transform") {
    val tree0 = "if (x) y else z".parse[Term].get
    val result1 = tree0 transform { case q"y" => "{ a + a }".parse[Term].get }
    val result2 = result1 transform { case q"a" => "{ a }".parse[Term].get }
    val result3 = result2 transform { case q"a" => "yes".parse[Term].get }
    val result4 = result3 transform { case q"yes" => "works".parse[Term].get }
    val s1 = "if (x) { { works } + { works } } else z"
    assert(s1 == result4.toString)
  }
  
  test("multiple statements within a block") {
    val tree0 = "if (x) y else z".parse[Term].get
    val result1 = tree0 transform { case q"y" => "{ foo1; foo2; foo3 }".parse[Term].get }
    val result2 = result1 transform { case q"foo1" => "works".parse[Term].get }
    val s1 = "if (x) { works; foo2; foo3 } else z"
    assert(s1 == result2.toString)
  }  

  test("preserve comments within a block") {
    val tree0 = "{         abc }".parse[Term].get
    val result1 = tree0 transform { case q"abc" => "bcdefg".parse[Term].get }
    val result2 = result1 transform { case q"bcdefg" => "asdf".parse[Term].get }  
    val result3 = result2 transform { case q"asdf" => "{ /* hello */ def foo(x: Int) = bar }".parse[Stat].get }
    val result4 = result3 transform { case q"foo" => "newfn".parse[Term].get }
    val s1 = "{         { /* hello */ def newfn(x: Int) = bar } }"
    assert(s1 == result4.toString)
  }

  test("change var to def statment") {
    val tree0 = "if (x) y else z".parse[Term].get
    val result1 = tree0 transform { case q"y" => "{                    abc }".parse[Term].get }
    val result2 = result1 transform { case q"abc" => "bcd".parse[Term].get }
    val result3 = result2 transform { case q"bcd" => "cde".parse[Term].get }
    val result4 = result3 transform { case q"cde" => "asdfasdf".parse[Term].get }
    val result5 = result4 transform { case q"asdfasdf" => "def foo(bar: Int) = baz".parse[Stat].get }
    val result6 = result5 transform { case q"foo" => "works".parse[Term].get }
    val s1 = "if (x) {                    def works(bar: Int) = baz } else z"
    assert(s1 == result6.toString)
  }

  test("val test") {
    val tree0 = "y".parse[Term].get
    val result1 = tree0 transform { case q"y" => "{ val a = m }".parse[Term].get }
    val result2 = result1 transform { case q"val a = m" => "val j = a".parse[Stat].get }
    val result3 = result2 transform { case q"j" => "b".parse[Term].get }
    val s1 = "{ val b = a }"
    assert(s1 == result3.toString)
  }

  test("class test1") {
    val tree0 = "y".parse[Term].get
    val result1 = tree0 transform { case q"y" => "/* comment out */ class C(x: Int) { def foo = bar }".parse[Stat].get }
    val result2 = result1 transform { case q"def foo = bar" => "{ /* hello */ def foo = baz }".parse[Stat].get }
    val result3 = result2 transform { case q"foo" => "baz".parse[Term].get }
    val result4 = result3 transform { case q"baz" => q"foo" }
    val result5 = result4 transform { case q"foo" => q"baz" }    
    val s1 = "/* comment out */ class C(x: Int) { { /* hello */ def baz = baz } }"
    assert(s1 == result5.toString)
  }

  test("transform stats within class") {
    val tree0 = "y".parse[Term].get
    val result1 = tree0 transform { case q"y" => "/* hello */ class C(x: Int) { def foo = bar; val x = baz }".parse[Stat].get }
    val result2 = result1 transform { case q"foo" => "baz".parse[Term].get }
    val result3 = result2 transform { case q"baz" => "foo".parse[Term].get }
    val result4 = result3 transform { case q"foo" => "baz".parse[Term].get }
    val result5 = result4 transform { case q"baz" => "foo".parse[Term].get }
    val s1 = "/* hello */ class C(x: Int) { def foo = bar; val x = foo }"
    assert(s1 == result5.toString)
  }

  test("modify def test1") {
    val tree0 = "y".parse[Term].get
    val result1 = tree0 transform { case q"y" => "def foo = bar".parse[Stat].get }
    val result2 = result1 transform { case q"def foo = bar" => "def foo = baz".parse[Stat].get }
    val result3 = result2 transform { case q"baz" => q"foo" }
    val s1 = "def foo = foo"
    assert(s1 == result3.toString)
  }

  test("modify def test2") {
    val tree0 = "{ def foo = bar }".parse[Term].get
    val result1 = tree0 transform { case q"def foo = bar" => "def foo = baz".parse[Stat].get }
    val result2 = result1 transform { case q"foo" => "bar".parse[Term].get }
    val result3 = result2 transform { case q"bar" => "foo".parse[Term].get }
    val s1 = "{ def foo = baz }"
    assert(s1 == result3.toString)
  }

  test("multiple match transform") {
    val tree0 = """
    x match {
      case a => b
      case c => d
      case _ => e
    }
    """.parse[Term].get
    val result1 = tree0 transform { case q"a" => "m".parse[Term].get }
    val result2 = result1 transform { case q"m" => "a".parse[Term].get }
    val s1 = """
    x match {
      case a => b
      case c => d
      case _ => e
    }
    """
    assert(s1 == result2.toString)
  }

  test("change stats within a class to match expr") {
    val tree0 = """
    class C(x: Int) {
      def foo = bar
      def foo = baz
      val x = 1
    }
    """.parse[Stat].get
    val result1 = tree0 transform { case q"val x = 1" => """x match { case 1 => 2 }""".parse[Term].get }
    val result2 = result1 transform { case q"x" => "y".parse[Term].get }
    val s1 = """
    class C(y: Int) {
      def foo = bar
      def foo = baz
      y match { case 1 => 2 }
    }
    """
    assert(s1 == result2.toString)
  }

  test("tricky infix transform") {
    val tree0 = "abc".parse[Term].get
    val res1 = tree0 transform { case q"abc" => "(x map y).foo".parse[Term].get }
    val res2 = res1 transform { case q"x" => "z".parse[Term].get }
    val res3 = res2 transform { case q"y" => "(y +  y )".parse[Term].get }
    val res4 = res3 transform { case q"y" => "a".parse[Term].get }
    val res5 = res4 transform { case q"a" => "b".parse[Term].get }
    val s1 = "(z map (b +  b )).foo"
    assert(s1 == res5.toString)
  }

  test("infix + block multiple transform") {
    val tree0 = "if (x) y else z".parse[Term].get
    val res1 = tree0 transform { case q"y" => "{ abc + abc }".parse[Term].get }
    val res2 = res1 transform { case q"abc" => "bcd".parse[Term].get }
    val res3 = res2 transform { case q"bcd" => "cde".parse[Term].get }
    val s1 = "if (x) { cde + cde } else z"
    assert(s1 == res3.toString)
  }

  test("preserve comment within unrelated transform") {
    val tree0 = """
    if (/* hello */ x) {
      y
    } else {
       z
    }""".parse[Term].get
    val res1 = tree0 transform { case q"y" => "/* some comment */ x + foo".parse[Term].get }
    val res2 = res1 transform { case q"foo" => "yes".parse[Term].get }
    val res3 = res2 transform { case q"z" => "foo + (x map y)".parse[Term].get }
    val s1 = """
    if (/* hello */ x) {
      /* some comment */ x + yes
    } else {
       foo + (x map y)
    }"""
    assert(s1 == res3.toString)
  }

  test("multiple stats in the result of match") {
    val tree0 = """
    x match {
      case y => { foo1; foo2 }
    }
    """.parse[Term].get
    val res1 = tree0 transform { case q"foo1" => "foo3".parse[Term].get }
    val res2 = res1 transform { case q"foo3" => "foo1".parse[Term].get }
    val s1 = """
    x match {
      case y => { foo1; foo2 }
    }
    """
    assert(s1 == res2.toString)
  }

  test("more infix trickiness1") {
    val tree0 = "if (x) y else z".parse[Term].get
    val res1 = tree0 transform { case q"y" => "{ a }".parse[Term].get }
    val res2 = res1 transform { case q"a" => "b + b + { b }".parse[Term].get }
    val res3 = res2 transform { case q"b" => "y".parse[Term].get }
    val res4 = res3 transform { case q"y" => "b".parse[Term].get }
    val s1 = "if (x) { b + b + { b } } else z"
    assert(s1 == res4.toString)
  }

  test("more infix trickiness2") {
    val tree0 = "if (x) y else z".parse[Term].get
    val res1 = tree0 transform { case q"y" => "{ (x map y).foo }".parse[Term].get }
    val res2 = res1 transform { case q"y" => "(y + y)".parse[Term].get }
    val res3 = res2 transform { case q"y" => "a".parse[Term].get }
    val s1 = "if (x) { (x map (a + a)).foo } else z"
    assert(s1 == res3.toString)
  }

  // some non-trivial refactorings coming up
  // examples taken from Mirko Stocker's thesis.
  test("attach types to methods") {
    val tree0 = "def foo(bar: Int) = bar".parse[Stat].get
    val res1 = tree0 transform { case tree0 => "/* some comment */ def foo(bar: Int): Int = baz".parse[Stat].get }
    val res2 = res1 transform { case q"baz" => "baz + { foo }".parse[Term].get }
    val res3 = res2 transform { case q"foo" => "baz".parse[Term].get }
    val res4 = res3 transform { case q"baz" => "foo".parse[Term].get }
    val s1 = "/* some comment */ def foo(bar: Int): Int = foo + { foo }"
    assert(s1 == res4.toString)
  }

  test("extract local (ish) rewrite") {
    val tree0 = """
    def main(args: Array[String]) {
      /* detecing OS */
      println("Detecting OS..")
      val props = System.getProperties

      if( props.get("os.name") == "Linux" ) {
        println("We’re on Linux!")
      } else { println("We’re not on Linux!") }
    }""".parse[Stat].get
    val res1 = tree0 transform { case tree0 =>
      """
    def main(args: Array[String]) {
      /* detecting OS */
      println("Detecting OS..")
      val props = System.getProperties
      val isLinux = props.get("os.name") == "Linux"

      if( isLinux ) {
        println("We’re on Linux!")
      } else { println("We’re not on Linux!") }
    }""".parse[Stat].get }
    val res2 = res1 transform { case q"isLinux" => "isMac".parse[Term].get }
    val res3 = res2 transform { case q"isMac" => "isLinux".parse[Term].get }
    val s1 =   """
    def main(args: Array[String]) {
      /* detecting OS */
      println("Detecting OS..")
      val props = System.getProperties
      val isLinux = props.get("os.name") == "Linux"

      if( isLinux ) {
        println("We’re on Linux!")
      } else { println("We’re not on Linux!") }
    }"""
    assert(s1 == res3.toString)
  }

  test("inline local (ish) rewrite") {
    val tree0 = """
    class Extr2 {
      def m {
        val five = 5 toString ;
        println(five)
        five + " is the answer"
      }
    }""".parse[Stat].get
    val res1 = tree0 transform { case tree0 =>
      """
      class Extr2 {
        def m {
          println(5 toString)
          (5 toString) + "is the answer"
        }
      }""".parse[Stat].get }
    val res2 = res1 transform { case q"5" => "(5 + 5)".parse[Term].get }
    val res3 = res2 transform { case q"5" => "(6 - 1)".parse[Term].get }
    val s1 = """
      class Extr2 {
        def m {
          println(((6 - 1) + (6 - 1)) toString)
          (((6 - 1) + (6 - 1)) toString) + "is the answer"
        }
      }"""
    assert(s1 == res3.toString)
  }

  test("throw with parens") {
    val tree0 = """def m { y }""".parse[Stat].get
    val res1 = tree0 transform { case q"y" => "(throw x)".parse[Term].get }
    val res2 = res1 transform { case q"x" => "y".parse[Term].get }
    val s1 = """def m { (throw y) }"""
    assert(s1 == res2.toString)
  }

  test("if/throw") {
    val tree0 = "if (x) y else z".parse[Term].get
    val res1 = tree0 transform { case q"y" => "(throw y)".parse[Term].get }
    val res2 = res1 transform { case q"y" => "b".parse[Term].get }
    val s1 = "if (x) (throw b) else z"
    assert(s1 == res2.toString)
  }

  test("if/fn apply") {
    val tree0 = "if (x) y else z".parse[Term].get
    val res1 = tree0 transform { case q"y" => "f(a)".parse[Term].get }
    val res2 = res1 transform { case q"a" => q"b" }
    val res3 = res2 transform { case q"b" => q"c" }
    val s1 = "if (x) f(c) else z"
    assert(s1 == res3.toString)
  }

  test("infix + fn apply") {
    val tree0 = "if (x) y else z".parse[Term].get
    val res1 = tree0 transform { case q"y" => "f(y)".parse[Term].get }
    val res2 = res1 transform { case q"y" => "y + y".parse[Term].get }
    val res3 = res2 transform { case q"y" => q"c" }
    val s1 = "if (x) f(c + c) else z"
    assert(s1 == res3.toString)
  }

  test("trait test") {
    val tree0 = "if (x) y else z".parse[Term].get
    val res1 = tree0 transform { case q"y" => "{ trait Foo[+T] { val bar = baz; val asdf = jklm } }".parse[Term].get }
    val res2 = res1 transform { case q"bar" => q"abc" }
    val res3 = res2 transform { case q"abc" => q"bcd" }
    val s1 = "if (x) { trait Foo[+T] { val bcd = baz; val asdf = jklm } } else z"
    assert(s1 == res3.toString)
  }

  test("class test1111") {
    val tree0 = "if (x) y else z".parse[Term].get
    val res1 = tree0 transform { case q"y" => "{ class C(x: Int) }".parse[Term].get }
    val res2 = res1 transform { case q"x" => q"y" }
    val s1 = "if (y) { class C(y: Int) } else z"
    assert(s1 == res2.toString)
  }

  test("tuple test multiple transform") {
    val tree0 = "if (x) y else z".parse[Term].get
    val res1 = tree0 transform { case q"y" => "(a, b)".parse[Term].get }
    val res2 = res1 transform { case q"a" => q"c" }
    val s1 = "if (x) (c, b) else z"
    assert(s1 == res2.toString)
  }

  test("multiple try catch test") {
    val tree0 = "a".parse[Term].get
    val res1 = tree0 transform { case tree0 =>
      """
      try {
        println(1)
      } catch {
        case e: Exception1 => x1
      }""".parse[Term].get }
    val res2 = res1 transform { case q"1" => q"2" }
    val res3 = res2 transform { case q"2" => q"3" }
    val s1 = """
      try {
        println(3)
      } catch {
        case e: Exception1 => x1
      }"""
    assert(s1 == res3.toString)
  }

  test("type test") {
    val tree0 = "a".parse[Term].get
    val res1 = tree0 transform { case tree0 => "trait Functor[F[_]]".parse[Stat].get }
    val res2 = res1 transform { case t"F" => t"G" }
    val res3 = res2 transform { case t"G" => t"H" }
    val s1 = "trait Functor[H[_]]"
    assert(s1 == res3.toString)
  }

  test("close brackets match") {
    val tree0 = "a".parse[Term].get
    val res1 = tree0 transform { case tree0 =>
      """
      (x, y) match {
        case (a: A, b: B) => z
        case _ => nothing
      }""".parse[Term].get }
    val res2 = res1 transform { case q"a" => q"c" }
    val res3 = res2 transform { case q"c" => q"d" }
    val s1 =   """
      (x, y) match {
        case (d: A, b: B) => z
        case _ => nothing
      }"""
    assert(s1 == res3.toString)
  }

  // // fails. 
  // test("simple test") {
  //   val tree0 = "/* hello */ a".parse[Term].get
  //   val res1 = tree0 transform { case q"a" => q"b" }
  //   val s1 = "/* hello */ b"

  //   assert(s1 == res1.toString)
  // }

  test("anon fn test") {
    val tree0 = "if (x) y else z".parse[Term].get
    val res1 = tree0 transform { case q"y" => "{ (a: Int) => a + 1 }".parse[Term].get }
    val res2 = res1 transform { case q"a" => q"b" }
    val res3 = res2 transform { case q"b" => q"c" }
    val s1 = "if (x) { (c: Int) => c + 1 } else z"
    assert(s1 == res3.toString)
  }

  test("type tuple multiple test") {
    val tree0 = "if (x) y else z".parse[Term].get
    val res1 = tree0 transform { case q"y" => "{ def foo(bar: (A, B)) = ??? }".parse[Term].get }
    val res2 = res1 transform { case t"A" => t"C" }
    val res3 = res2 transform { case t"C" => t"D" }
    val s1 = "if (x) { def foo(bar: (D, B)) = ??? } else z"
    assert(s1 == res3.toString)
  }

  test("simple test") {
    val tree0 = "a".parse[Term].get
    val res1 = tree0 transform { case tree0 => "class C(x: Int)".parse[Stat].get }
    val res2 = res1 transform { case q"x" => q"y" }
    val res3 = res2 transform { case q"y" => q"z" }
    val s1 = "class C(z: Int)"
    assert(s1 == res3.toString)
  }

  test("simple test1") {
    val tree0 = "a".parse[Term].get
    val res1 = tree0 transform { case q"a" => "def foo(bar: Int) = baz".parse[Stat].get }
    val res2 = res1 transform { case q"bar" => q"baz" }
    val res3 = res2 transform { case q"baz" => q"bar" }
    val s1 = "def foo(bar: Int) = bar"
    assert(s1 == res3.toString)
  }

  test("simple test2") {
    val tree0 = """
    {
      val x = 1
      val y = 2
      val z = 3
    }""".parse[Term].get
    val res1 = tree0 transform { case q"val y = 2" => "def foo = bar".parse[Stat].get }
    val res2 = res1 transform { case q"bar" => q"abc" }
    val s1 = """
    {
      val x = 1
      def foo = abc
      val z = 3
    }"""
    assert(s1 == res2.toString)
  }

  test("comment preserving test") {
    val tree0 = "y".parse[Term].get
    val res1 = tree0 transform { case q"y" => "def foo(bar: Int) = baz /* hello */".parse[Stat].get }
    val res2 = res1 transform { case q"baz" => q"abc" }
    val res3 = res2 transform { case q"abc" => q"bcd" }
    val s1 = "def foo(bar: Int) = bcd /* hello */"
    assert(s1 == res3.toString)
  }

  test("comment preserving test1") {
    val tree0 = """
    {
      val x = 1
      val y = 2
      val z = 3
    }""".parse[Term].get
    val res1 = tree0 transform { case q"val y = 2" => "def foo = bar /* hello */".parse[Stat].get }
    val res2 = res1 transform { case q"bar" => q"baz" }
    val res3 = res2 transform { case q"baz" => q"bar" }
    val res4 = res3 transform { case q"bar" => "baz + baz".parse[Term].get }
    val s1 = """
    {
      val x = 1
      def foo = baz + baz /* hello */
      val z = 3
    }"""
    assert(s1 == res4.toString)
  }

  test("comment within block test") {
    val tree0 = "if (x) y else z".parse[Term].get
    val res1 = tree0 transform { case q"y" => "{     val foo = abc /* hey */ }".parse[Term].get }
    val res2 = res1 transform { case q"abc" => "bcd /* hey */".parse[Term].get }
    val res3 = res2 transform { case q"bcd" => q"efg" }
    val s1 = "if (x) {     val foo = efg /* hey */ } else z"
    assert(s1 == res3.toString)
  }

  test("comment before )") {
    val tree0 = "a".parse[Term].get
    val res1 = tree0 transform { case q"a" => "(throw x /* lol */)".parse[Term].get }
    val res2 = res1 transform { case q"x" => q"y" }
    val res3 = res2 transform { case q"y" => q"z" }
    val s1 = "(throw z /* lol */)"
    assert(s1 == res3.toString)
  }
}
