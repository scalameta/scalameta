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
  check("_empty_/")(_.isPackage)
  check("_root_/")(_.isPackage)
  check("java/util/ArrayList#size.")(_.isField)
  check("java/util/Map#Entry#")(_.isInterface)
  check("scala/Any#asInstanceOf().")(_.isMethod)
  check("scala/AnyRef#")(_.isClass)
  check("scala/Option#[A]")(_.isTypeParameter)
  check("scala/Predef.assert().(assertion)")(_.isParameter)
  check("scala/Predef.assume().")(_.isMethod)
  check("scala/collection/immutable/`::`.")(_.isObject)
  check("scala/collection/mutable/StringBuilder#`<init>`().")(_.isConstructor)
  check("scala/concurrent/Future#")(_.isTrait)
  check("scala/package.")(_.isPackageObject)
  check("scala/package.Either#")(_.isType)
  check("scala/package.Either.")(_.isMethod)
  check("scala/reflect/package.materializeClassTag().")(_.isMacro)
  check("scala/util/")(_.isPackage)

  // directory classpath entries
  check("example/Example.")(_.isObject)
  check("_empty_/A#")(_.isClass)
  check("org/scalameta/data/data#")(_.isClass)

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
    assert(local0.isType)
    assert(local0.displayName == "_")
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
