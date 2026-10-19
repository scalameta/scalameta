package scala.meta.tests.semanticdb

import scala.meta.internal.{semanticdb => s}

// https://github.com/scalameta/scalameta/pull/4916
class Issue4916Suite extends SemanticdbSuite {

  private val members =
    """|package a
       |class C {
       |  val inferredVal = Class.forName("a.C")
       |  def inferredDef = Class.forName("a.C")
       |  val boundedVal = "".getClass
       |  val rawVal = java.util.Collections.EMPTY_LIST
       |  val nestedVal = List(Class.forName("a.C"))
       |  def wildcardArgDef(k: java.nio.file.WatchKey) = k.pollEvents()
       |  val placeholderVal: Class[_] = ???
       |  val forSomeVal: Class[T] forSome { type T } = ???
       |  def forSomeDef: Class[T] forSome { type T } = ???
       |}
       |""".stripMargin

  private val locals =
    """|package b
       |class C {
       |  def m(): Unit = {
       |    val inferredVal = Class.forName("b.C")
       |    val placeholderVal: Class[_] = ???
       |    val forSomeVal: Class[T] forSome { type T } = ???
       |  }
       |}
       |""".stripMargin

  private val siblings =
    """|package c
       |class C {
       |  def ownDef: List[List[_]] = ???
       |  val ownVal = ownDef
       |  val scalaDefVal = new example.ExistentialsE().m
       |  val scalaForSomeVal = new advanced.Existential().e2
       |  val structuralVal = new advanced.Structural().s1
       |  val structuralX = structuralVal.x
       |  def javaDef(t: java.lang.invoke.MethodType) = t.parameterList()
       |  val javaVal = java.lang.invoke.MethodType.genericMethodType(0).parameterList()
       |  val boundedVal = java.lang.management.ManagementFactory.getPlatformManagementInterfaces()
       |  val collectorVal = java.util.stream.Collectors.toList[String]()
       |  def m(): Unit = {
       |    val localVal = java.lang.invoke.MethodType.genericMethodType(0).parameterList()
       |    val lambdaVal = (t: java.lang.invoke.MethodType) => t.parameterList()
       |  }
       |}
       |""".stripMargin

  private val structural =
    """|package d
       |class C {
       |  val structuralVal = new advanced.Structural().s1
       |  val structuralX = structuralVal.x
       |}
       |""".stripMargin

  /* For each symbol, the existential declarations that its signature
   * hardlinks, at any depth, and whether the Symbols section has them too.
   */
  private def hardlinks(doc: s.TextDocument): List[String] = {
    val inSymbols = doc.symbols.map(_.symbol).toSet
    def tpe(t: s.Type): List[s.SymbolInformation] = t match {
      case t: s.ExistentialType => t.declarations.toList.flatMap(_.hardlinks) ++ tpe(t.tpe)
      case t: s.TypeRef => tpe(t.prefix) ++ t.typeArguments.flatMap(tpe)
      case _ => Nil
    }
    doc.symbols.toList.flatMap { info =>
      val links = info.signature match {
        case sig: s.ValueSignature => tpe(sig.tpe)
        case sig: s.MethodSignature => tpe(sig.returnType)
        case _ => Nil
      }
      links.map { link =>
        val where =
          if (link.symbol.isEmpty) "no symbol"
          else if (inSymbols(link.symbol)) "in Symbols"
          else "hardlink only"
        s"${info.displayName} => ${link.displayName}: $where"
      }
    }.sorted
  }

  symbols(
    members,
    """|a/C# => class C extends AnyRef { +10 decls }
       |  AnyRef => scala/AnyRef#
       |a/C#`<init>`(). => primary ctor <init>()
       |a/C#boundedVal. => val method boundedVal: Class[T] forSome { type T <: String }
       |  Class => java/lang/Class#
       |  T => local2
       |  String => java/lang/String#
       |a/C#forSomeDef(). => method forSomeDef: Class[T] forSome { type T }
       |  Class => scala/Predef.Class#
       |  T => local9
       |a/C#forSomeVal. => val method forSomeVal: Class[T] forSome { type T }
       |  Class => scala/Predef.Class#
       |  T => local8
       |a/C#inferredDef(). => method inferredDef: Class[?0] forSome { type ?0 }
       |  Class => java/lang/Class#
       |  ?0 => local1
       |a/C#inferredVal. => val method inferredVal: Class[?0] forSome { type ?0 }
       |  Class => java/lang/Class#
       |  ?0 => local0
       |a/C#nestedVal. => val method nestedVal: List[Class[?0]] forSome { type ?0 }
       |  List => scala/collection/immutable/List#
       |  Class => java/lang/Class#
       |  ?0 => local5
       |a/C#placeholderVal. => val method placeholderVal: Class[_] forSome { type _ }
       |  Class => scala/Predef.Class#
       |  _ => local7
       |a/C#rawVal. => val method rawVal: List[E] forSome { type E }
       |  List => java/util/List#
       |  E => local3
       |a/C#wildcardArgDef(). => method wildcardArgDef(k: WatchKey): List[WatchEvent[?0] forSome { type ?0 }]
       |  k => a/C#wildcardArgDef().(k)
       |  WatchKey => java/nio/file/WatchKey#
       |  List => java/util/List#
       |  WatchEvent => java/nio/file/WatchEvent#
       |  ?0 => local6
       |a/C#wildcardArgDef().(k) => param k: WatchKey
       |  WatchKey => java/nio/file/WatchKey#
       |local7 => synthetic abstract type _
       |local8 => abstract type T
       |local9 => abstract type T
       |""".stripMargin,
  )

  targeted(
    members,
    doc =>
      assertEquals(
        hardlinks(doc),
        List(
          "boundedVal => T: hardlink only",
          "forSomeDef => T: in Symbols",
          "forSomeVal => T: in Symbols",
          "inferredDef => ?0: hardlink only",
          "inferredVal => ?0: hardlink only",
          "nestedVal => ?0: hardlink only",
          "placeholderVal => _: in Symbols",
          "rawVal => E: hardlink only",
          "wildcardArgDef => ?0: hardlink only",
        ),
      ),
  )

  symbols(
    locals,
    """|b/C# => class C extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |b/C#`<init>`(). => primary ctor <init>()
       |b/C#m(). => method m(): Unit
       |  Unit => scala/Unit#
       |local0 => val local inferredVal: Class[?0] forSome { type ?0 }
       |  Class => java/lang/Class#
       |  ?0 => local1
       |local2 => val local placeholderVal: Class[_] forSome { type _ }
       |  Class => scala/Predef.Class#
       |  _ => local3
       |local3 => synthetic abstract type _
       |local4 => val local forSomeVal: Class[T] forSome { type T }
       |  Class => scala/Predef.Class#
       |  T => local5
       |local5 => abstract type T
       |""".stripMargin,
  )

  targeted(
    locals,
    doc =>
      assertEquals(
        hardlinks(doc),
        List(
          "forSomeVal => T: in Symbols",
          "inferredVal => ?0: hardlink only",
          "placeholderVal => _: in Symbols",
        ),
      ),
  )

  symbols(
    siblings,
    """|c/C# => class C extends AnyRef { +12 decls }
       |  AnyRef => scala/AnyRef#
       |c/C#`<init>`(). => primary ctor <init>()
       |c/C#boundedVal. => val method boundedVal: Set[Class[?0] forSome { type ?0 <: PlatformManagedObject }]
       |  Set => java/util/Set#
       |  Class => java/lang/Class#
       |  ?0 => local5
       |  PlatformManagedObject => java/lang/management/PlatformManagedObject#
       |c/C#collectorVal. => val method collectorVal: Collector[String, ?0, List[String]] forSome { type ?0 }
       |  Collector => java/util/stream/Collector#
       |  String => java/lang/String#
       |  ?0 => local6
       |  List => java/util/List#
       |c/C#javaDef(). => method javaDef(t: MethodType): List[Class[?0] forSome { type ?0 }]
       |  t => c/C#javaDef().(t)
       |  MethodType => java/lang/invoke/MethodType#
       |  List => java/util/List#
       |  Class => java/lang/Class#
       |  ?0 => local4
       |c/C#javaDef().(t) => param t: MethodType
       |  MethodType => java/lang/invoke/MethodType#
       |c/C#javaVal. => val method javaVal: List[Class[?0] forSome { type ?0 }]
       |  List => java/util/List#
       |  Class => java/lang/Class#
       |  ?0 => local4
       |c/C#m(). => method m(): Unit
       |  Unit => scala/Unit#
       |c/C#ownDef(). => method ownDef: List[List[_] forSome { type _ }]
       |  List => scala/package.List#
       |  _ => local0
       |c/C#ownVal. => val method ownVal: List[List[_] forSome { type _ }]
       |  List => scala/package.List#
       |  _ => local0
       |c/C#scalaDefVal. => val method scalaDefVal: List[List[_] forSome { type _ }]
       |  List => scala/package.List#
       |  _ => local1
       |c/C#scalaForSomeVal. => val method scalaForSomeVal: C[List[T] forSome { type T }]
       |  C => advanced/C#
       |  List => scala/package.List#
       |  T => local2
       |c/C#structuralVal. => val method structuralVal: AnyRef { val def x: Int }
       |  AnyRef => scala/AnyRef#
       |  x => local3
       |  Int => scala/Int#
       |c/C#structuralX. => val method structuralX: Int
       |  Int => scala/Int#
       |local0 => synthetic abstract type _
       |local7 => val local localVal: List[Class[?0] forSome { type ?0 }]
       |  List => java/util/List#
       |  Class => java/lang/Class#
       |  ?0 => local4
       |local8 => param t: MethodType
       |  MethodType => java/lang/invoke/MethodType#
       |local9 => val local lambdaVal: Function1[MethodType, List[Class[?0] forSome { type ?0 }]]
       |  Function1 => scala/Function1#
       |  MethodType => java/lang/invoke/MethodType#
       |  List => java/util/List#
       |  Class => java/lang/Class#
       |  ?0 => local4
       |""".stripMargin,
  )

  targeted(
    siblings,
    doc =>
      assertEquals(
        hardlinks(doc),
        List(
          "boundedVal => ?0: hardlink only",
          "collectorVal => ?0: hardlink only",
          "javaDef => ?0: hardlink only",
          "javaVal => ?0: hardlink only",
          "lambdaVal => ?0: hardlink only",
          "localVal => ?0: hardlink only",
          "ownDef => _: in Symbols",
          "ownVal => _: in Symbols",
          "scalaDefVal => _: hardlink only",
          "scalaForSomeVal => T: hardlink only",
        ),
      ),
  )

  occurrences(
    structural,
    """|[0:8..0:9): d <= d/
       |[1:6..1:7): C <= d/C#
       |[1:8..1:8):  <= d/C#`<init>`().
       |[2:6..2:19): structuralVal <= d/C#structuralVal.
       |[2:26..2:34): advanced => advanced/
       |[2:35..2:45): Structural => advanced/Structural#
       |[2:45..2:45):  => advanced/Structural#`<init>`().
       |[2:48..2:50): s1 => advanced/Structural#s1().
       |[3:6..3:17): structuralX <= d/C#structuralX.
       |[3:20..3:33): structuralVal => d/C#structuralVal.
       |[3:34..3:35): x => local0
       |""".stripMargin,
  )

}
