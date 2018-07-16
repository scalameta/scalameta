package scala.meta.tests.symtab

import org.scalatest.FunSuite
import scala.meta.internal.symtab.AggregateSymbolTable
import scala.meta.internal.symtab.GlobalSymbolTable
import scala.meta.internal.symtab.LocalSymbolTable
import scala.meta.tests.metacp.Library
import scala.meta.internal.{semanticdb => s}
import scala.meta.io.Classpath
import scala.meta.tests.BuildInfo

class SymbolTableSuite extends FunSuite {
  private val classpath =
    Classpath(BuildInfo.databaseClasspath) ++
      Classpath(BuildInfo.commonJVMClassDirectory) ++
      Library.jdk.classpath() ++
      Library.scalaLibrary.classpath()
  private val globalSymtab = GlobalSymbolTable(classpath)

  def checkNotExists(symbol: String): Unit = {
    val name = if (symbol.isEmpty) "<nosymbol>" else symbol
    test(name) {
      val obtained = globalSymtab.info(symbol)
      assert(obtained.isEmpty, symbol)
    }
  }

  def check(symbol: String)(f: s.SymbolInformation => Boolean): Unit = {
    test(symbol) {
      val obtained = globalSymtab.info(symbol)
      assert(obtained.nonEmpty, symbol)
      assert(f(obtained.get), obtained.get.toProtoString)
    }
  }

  // jar classpath entries
  check("_empty_/")(_.kind.isPackage)
  check("_root_/")(_.kind.isPackage)
  check("java/util/ArrayList#size.")(_.kind.isField)
  check("java/util/Map#Entry#")(_.kind.isInterface)
  check("scala/Any#asInstanceOf().")(_.kind.isMethod)
  check("scala/AnyRef#")(_.kind.isClass)
  check("scala/Option#[A]")(_.kind.isTypeParameter)
  check("scala/Predef.assert().(assertion)")(_.kind.isParameter)
  check("scala/Predef.assume().")(_.kind.isMethod)
  check("scala/collection/immutable/`::`.")(_.kind.isObject)
  check("scala/collection/mutable/StringBuilder#`<init>`().")(_.kind.isConstructor)
  check("scala/concurrent/Future#")(_.kind.isTrait)
  check("scala/package.")(_.kind.isPackageObject)
  check("scala/package.Either#")(_.kind.isType)
  check("scala/package.Either.")(_.kind.isMethod)
  check("scala/reflect/package.materializeClassTag().")(_.kind.isMacro)
  check("scala/util/")(_.kind.isPackage)

  // directory classpath entries
  check("example/Example.")(_.kind.isObject)
  check("_empty_/A#")(_.kind.isClass)
  check("org/scalameta/data/data#")(_.kind.isClass)

  checkNotExists("")
  checkNotExists("local20")
  checkNotExists("foo/bar/")
  checkNotExists("foo.bar/")
  checkNotExists("does/not/Exist#")
  checkNotExists("does/not/Exist.")
  checkNotExists("does/not/Exist.(a)")
  checkNotExists("does/not/`e-x-i-s-t`#")

  test("LocalSymbolTable.info") {
    val runtimeClass = globalSymtab.info("scala/reflect/ClassTag#runtimeClass().").get
    val returnType = runtimeClass.signature.asInstanceOf[s.MethodSignature].returnType
    val scope = returnType.asInstanceOf[s.ExistentialType].declarations.get
    val localSymtab = LocalSymbolTable(scope.hardlinks)
    val aggregateSymtab = AggregateSymbolTable(List(localSymtab, globalSymtab))
    val local0 = aggregateSymtab.info("local0").get
    assert(local0.kind.isType)
    assert(local0.name == "_")
  }

  test("SymbolTable.toString") {
    val localSymtab = LocalSymbolTable(List(s.SymbolInformation("local3")))
    val aggregateSymbolTable = AggregateSymbolTable(List(localSymtab, globalSymtab))
    val string = aggregateSymbolTable.toString
    assert(string.contains("LocalSymbolTable"))
    assert(string.contains("GlobalSymbolTable"))
    assert(string.contains("AggregateSymbolTable"))
    assert(string.contains("entries"))
  }

}
