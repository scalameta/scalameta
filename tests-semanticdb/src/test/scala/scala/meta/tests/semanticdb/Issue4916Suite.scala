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

  private val listApply =
    if (isScala213) "scala/collection/IterableFactory#apply()."
    else "scala/collection/immutable/List.apply()."

  checkSection(
    "members: synthetics",
    members,
    s"""|[6:18..6:22): List => *.apply[Class[local4]]
        |  apply => $listApply
        |  Class => java/lang/Class#
        |  local4 => local4
        |""".stripMargin,
    "Synthetics",
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

  // two skolems of one name in one value, which scalac packs into two binders of that name
  private val twoSkolems =
    """|package e
       |class C {
       |  val twoVal = List(Class.forName("e.C"), Class.forName("e.D"))
       |}
       |""".stripMargin

  checkSection(
    "twoSkolems: synthetics",
    twoSkolems,
    s"""|[2:15..2:19): List => *.apply[Class[_1] forSome { type _1 >: local1 with local2 {} }]
        |  apply => $listApply
        |  Class => java/lang/Class#
        |  _1 => local0
        |  local1 => local1
        |  local2 => local2
        |""".stripMargin,
    "Synthetics",
  )

  symbols(
    twoSkolems,
    """|e/C# => class C extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |e/C#`<init>`(). => primary ctor <init>()
       |e/C#twoVal. => val method twoVal: List[Class[_1] forSome { type _1 >: ?0 with ?0 {} }] forSome { type ?0; type ?0 }
       |  List => scala/collection/immutable/List#
       |  Class => java/lang/Class#
       |  _1 => local0
       |  ?0 => local3
       |  ?0 => local4
       |""".stripMargin,
  )

  // two skolems of one name in a def, a lazy val, a var and a local value
  private val twoSkolemMembers =
    """|package k
       |class C {
       |  def twoDef = List(Class.forName("k.C"), Class.forName("k.D"))
       |  lazy val twoLazy = List(Class.forName("k.C"), Class.forName("k.D"))
       |  var twoVar = List(Class.forName("k.C"), Class.forName("k.D"))
       |  def m(): Unit = { val twoLocal = List(Class.forName("k.C"), Class.forName("k.D")) }
       |}
       |""".stripMargin

  checkSection(
    "twoSkolemMembers: synthetics",
    twoSkolemMembers,
    s"""|[2:15..2:19): List => *.apply[Class[_1] forSome { type _1 >: local3 with local4 {} }]
        |  apply => $listApply
        |  Class => java/lang/Class#
        |  _1 => local0
        |  local3 => local3
        |  local4 => local4
        |[3:21..3:25): List => *.apply[Class[_1] forSome { type _1 >: local6 with local7 {} }]
        |  apply => $listApply
        |  Class => java/lang/Class#
        |  _1 => local5
        |  local6 => local6
        |  local7 => local7
        |[4:15..4:19): List => *.apply[Class[_1] forSome { type _1 >: local11 with local12 {} }]
        |  apply => $listApply
        |  Class => java/lang/Class#
        |  _1 => local10
        |  local11 => local11
        |  local12 => local12
        |[5:35..5:39): List => *.apply[Class[_1] forSome { type _1 >: local16 with local17 {} }]
        |  apply => $listApply
        |  Class => java/lang/Class#
        |  _1 => local15
        |  local16 => local16
        |  local17 => local17
        |""".stripMargin,
    "Synthetics",
  )

  symbols(
    twoSkolemMembers,
    """|k/C# => class C extends AnyRef { +6 decls }
       |  AnyRef => scala/AnyRef#
       |k/C#`<init>`(). => primary ctor <init>()
       |k/C#`twoVar_=`(). => var method twoVar_=(x$1: List[Class[_1] forSome { type _1 >: ?0 with ?0 {} }] forSome { type ?0; type ?0 }): Unit
       |  x$1 => k/C#`twoVar_=`().(x$1)
       |  List => scala/collection/immutable/List#
       |  Class => java/lang/Class#
       |  _1 => local10
       |  ?0 => local13
       |  ?0 => local14
       |  Unit => scala/Unit#
       |k/C#`twoVar_=`().(x$1) => synthetic param x$1: List[Class[_1] forSome { type _1 >: ?0 with ?0 {} }] forSome { type ?0; type ?0 }
       |  List => scala/collection/immutable/List#
       |  Class => java/lang/Class#
       |  _1 => local10
       |  ?0 => local13
       |  ?0 => local14
       |k/C#m(). => method m(): Unit
       |  Unit => scala/Unit#
       |k/C#twoDef(). => method twoDef: List[Class[_1] forSome { type _1 >: ?0 with ?0 {} }] forSome { type ?0; type ?0 }
       |  List => scala/collection/immutable/List#
       |  Class => java/lang/Class#
       |  _1 => local0
       |  ?0 => local1
       |  ?0 => local2
       |k/C#twoLazy. => lazy val method twoLazy: List[Class[_1] forSome { type _1 >: ?0 with ?0 {} }] forSome { type ?0; type ?0 }
       |  List => scala/collection/immutable/List#
       |  Class => java/lang/Class#
       |  _1 => local5
       |  ?0 => local8
       |  ?0 => local9
       |k/C#twoVar(). => var method twoVar: List[Class[_1] forSome { type _1 >: ?0 with ?0 {} }] forSome { type ?0; type ?0 }
       |  List => scala/collection/immutable/List#
       |  Class => java/lang/Class#
       |  _1 => local10
       |  ?0 => local13
       |  ?0 => local14
       |local18 => val local twoLocal: List[Class[_1] forSome { type _1 >: ?0 with ?0 {} }] forSome { type ?0; type ?0 }
       |  List => scala/collection/immutable/List#
       |  Class => java/lang/Class#
       |  _1 => local15
       |  ?0 => local19
       |  ?0 => local20
       |""".stripMargin,
  )

  // a skolem that scalac replaces by its bound, beside a skolem of the same name that it packs
  private val droppedSkolems =
    """|package f
       |class C {
       |  def f[T](c: Class[T]): T = ???
       |  val short = (f(Class.forName("f.C").asSubclass(classOf[Runnable])), Class.forName("f.D"))
       |  val long = (Class.forName("f.D"), f(Class.forName("f.C").asSubclass(classOf[java.util.List[String]])))
       |}
       |""".stripMargin

  checkSection(
    "droppedSkolems: synthetics",
    droppedSkolems,
    s"""|[3:15..3:16): f => *[local0]
        |  local0 => local0
        |[3:17..3:48): Class.forName("f.C").asSubclass => *[Runnable]
        |  Runnable => java/lang/Runnable#
        |[4:36..4:37): f => *[local2]
        |  local2 => local2
        |[4:38..4:69): Class.forName("f.C").asSubclass => *[List[String]]
        |  List => java/util/List#
        |  String => scala/Predef.String#
        |""".stripMargin,
    "Synthetics",
  )

  symbols(
    droppedSkolems,
    """|f/C# => class C extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |f/C#`<init>`(). => primary ctor <init>()
       |f/C#f(). => method f[T](c: Class[T]): T
       |  T => f/C#f().[T]
       |  c => f/C#f().(c)
       |  Class => scala/Predef.Class#
       |f/C#f().(c) => param c: Class[T]
       |  Class => scala/Predef.Class#
       |  T => f/C#f().[T]
       |f/C#f().[T] => typeparam T
       |f/C#long. => val method long: Tuple2[Class[?0], List[String]] forSome { type ?0 }
       |  Tuple2 => scala/Tuple2#
       |  Class => java/lang/Class#
       |  ?0 => local3
       |  List => java/util/List#
       |  String => scala/Predef.String#
       |f/C#short. => val method short: Tuple2[Runnable, Class[?0]] forSome { type ?0 }
       |  Tuple2 => scala/Tuple2#
       |  Runnable => java/lang/Runnable#
       |  Class => java/lang/Class#
       |  ?0 => local1
       |""".stripMargin,
  )

  // skolems in the body of a function, which scalac packs into the result type of the function
  private val functionSkolems =
    """|package g
       |class C {
       |  val one = (x: Int) => List(Class.forName("g.C"))
       |  val two = (x: Int) => List(Class.forName("g.C"), Class.forName("g.D"))
       |}
       |""".stripMargin

  checkSection(
    "functionSkolems: synthetics",
    functionSkolems,
    s"""|[2:24..2:28): List => *.apply[Class[local1]]
        |  apply => $listApply
        |  Class => java/lang/Class#
        |  local1 => local1
        |[3:24..3:28): List => *.apply[Class[_1] forSome { type _1 >: local5 with local6 {} }]
        |  apply => $listApply
        |  Class => java/lang/Class#
        |  _1 => local4
        |  local5 => local5
        |  local6 => local6
        |""".stripMargin,
    "Synthetics",
  )

  symbols(
    functionSkolems,
    """|g/C# => class C extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |g/C#`<init>`(). => primary ctor <init>()
       |g/C#one. => val method one: Function1[Int, List[Class[?0]] forSome { type ?0 }]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
       |  Class => java/lang/Class#
       |  ?0 => local2
       |g/C#two. => val method two: Function1[Int, List[Class[_1] forSome { type _1 >: ?0 with ?0 {} }] forSome { type ?0; type ?0 }]
       |  Function1 => scala/Function1#
       |  Int => scala/Int#
       |  List => scala/collection/immutable/List#
       |  Class => java/lang/Class#
       |  _1 => local4
       |  ?0 => local7
       |  ?0 => local8
       |local0 => param x: Int
       |  Int => scala/Int#
       |local3 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  // skolems in a type alias, which scalac expands in the packed type
  private val aliasSkolems =
    """|package h
       |class C {
       |  type Pair[T] = (T, T)
       |  def h[T](c: Class[T]): Pair[T] = ???
       |  val p = (h(Class.forName("h.C")), h(Class.forName("h.D")))
       |}
       |""".stripMargin

  checkSection(
    "aliasSkolems: synthetics",
    aliasSkolems,
    s"""|[4:11..4:12): h => *[local0]
        |  local0 => local0
        |[4:36..4:37): h => *[local1]
        |  local1 => local1
        |""".stripMargin,
    "Synthetics",
  )

  symbols(
    aliasSkolems,
    """|h/C# => class C extends AnyRef { +4 decls }
       |  AnyRef => scala/AnyRef#
       |h/C#Pair# => type Pair[T] = Tuple2[T, T]
       |  T => h/C#Pair#[T]
       |  Tuple2 => scala/Tuple2#
       |h/C#Pair#[T] => typeparam T
       |h/C#`<init>`(). => primary ctor <init>()
       |h/C#h(). => method h[T](c: Class[T]): Pair[T]
       |  T => h/C#h().[T]
       |  c => h/C#h().(c)
       |  Class => scala/Predef.Class#
       |  Pair => h/C#Pair#
       |h/C#h().(c) => param c: Class[T]
       |  Class => scala/Predef.Class#
       |  T => h/C#h().[T]
       |h/C#h().[T] => typeparam T
       |h/C#p. => val method p: Tuple2[Tuple2[?0, ?0], Tuple2[?0, ?0]] forSome { type ?0; type ?0 }
       |  Tuple2 => scala/Tuple2#
       |  ?0 => local2
       |  ?0 => local3
       |""".stripMargin,
  )

  // skolems of a match case or of a block statement, which the type of the value does not hold
  private val chainedSkolems =
    """|package i
       |class C {
       |  def f[T](c: Class[T]): T = ???
       |  val m = 1 match { case _ => List(Class.forName("i.C")) }
       |  val b = { f(Class.forName("i.C")); List(Class.forName("i.D")) }
       |  val n = 1 match { case 1 => List(Class.forName("i.C")); case _ => List(Class.forName("i.D")) }
       |}
       |""".stripMargin

  checkSection(
    "chainedSkolems: synthetics",
    chainedSkolems,
    s"""|[3:30..3:34): List => *.apply[Class[local0]]
        |  apply => $listApply
        |  Class => java/lang/Class#
        |  local0 => local0
        |[4:12..4:13): f => *[local2]
        |  local2 => local2
        |[4:37..4:41): List => *.apply[Class[local3]]
        |  apply => $listApply
        |  Class => java/lang/Class#
        |  local3 => local3
        |[5:30..5:34): List => *.apply[Class[local5]]
        |  apply => $listApply
        |  Class => java/lang/Class#
        |  local5 => local5
        |[5:68..5:72): List => *.apply[Class[local6]]
        |  apply => $listApply
        |  Class => java/lang/Class#
        |  local6 => local6
        |""".stripMargin,
    "Synthetics",
  )

  symbols(
    chainedSkolems,
    """|i/C# => class C extends AnyRef { +5 decls }
       |  AnyRef => scala/AnyRef#
       |i/C#`<init>`(). => primary ctor <init>()
       |i/C#b. => val method b: List[Class[?0]] forSome { type ?0 }
       |  List => scala/collection/immutable/List#
       |  Class => java/lang/Class#
       |  ?0 => local4
       |i/C#f(). => method f[T](c: Class[T]): T
       |  T => i/C#f().[T]
       |  c => i/C#f().(c)
       |  Class => scala/Predef.Class#
       |i/C#f().(c) => param c: Class[T]
       |  Class => scala/Predef.Class#
       |  T => i/C#f().[T]
       |i/C#f().[T] => typeparam T
       |i/C#m. => val method m: List[Class[?0]] forSome { type ?0 }
       |  List => scala/collection/immutable/List#
       |  Class => java/lang/Class#
       |  ?0 => local1
       |i/C#n. => val method n: List[Class[?0]] forSome { type ?0 }
       |  List => scala/collection/immutable/List#
       |  Class => java/lang/Class#
       |  ?0 => local7
       |""".stripMargin,
  )

  // a skolem in the body of a SAM function
  private val samSkolems =
    """|package l
       |class C {
       |  val sam: java.util.function.IntFunction[Any] = (x: Int) => List(Class.forName("l.C"))
       |}
       |""".stripMargin

  checkSection(
    "samSkolems: synthetics",
    samSkolems,
    s"""|[2:61..2:65): List => *.apply[Class[local4]]
        |  apply => $listApply
        |  Class => java/lang/Class#
        |  local4 => local4
        |""".stripMargin,
    "Synthetics",
  )

  symbols(
    samSkolems,
    """|l/C# => class C extends AnyRef { +2 decls }
       |  AnyRef => scala/AnyRef#
       |l/C#`<init>`(). => primary ctor <init>()
       |l/C#sam. => val method sam: IntFunction[Any]
       |  IntFunction => java/util/function/IntFunction#
       |  Any => scala/Any#
       |local0 => synthetic class $anon extends Object with IntFunction[Any] { +1 decls }
       |  Object => java/lang/Object#
       |  IntFunction => java/util/function/IntFunction#
       |  Any => scala/Any#
       |local2 => method apply(unknown x$1): Any <: java/util/function/IntFunction#apply().
       |  x$1 => java/util/function/IntFunction#apply().(x$1)
       |  Any => scala/Any#
       |local3 => param x: Int
       |  Int => scala/Int#
       |""".stripMargin,
  )

  // two skolems of one name in a value whose explicit type has one binder of that name
  private val sharedBinder =
    """|package j
       |class C {
       |  val e = new example.ExistentialsE()
       |  val v: (T, T) forSome { type T } = Tuple2(e.f, e.f)
       |}
       |""".stripMargin

  checkSection(
    "sharedBinder: synthetics",
    sharedBinder,
    """|[3:37..3:43): Tuple2 => *.apply[local0, local1]
       |  apply => scala/Tuple2.apply().
       |  local0 => local0
       |  local1 => local1
       |""".stripMargin,
    "Synthetics",
  )

  symbols(
    sharedBinder,
    """|j/C# => class C extends AnyRef { +3 decls }
       |  AnyRef => scala/AnyRef#
       |j/C#`<init>`(). => primary ctor <init>()
       |j/C#e. => val method e: ExistentialsE
       |  ExistentialsE => example/ExistentialsE#
       |j/C#v. => val method v: Tuple2[T, T] forSome { type T }
       |  Tuple2 => scala/Tuple2#
       |  T => local2
       |local2 => abstract type T
       |""".stripMargin,
  )

}
