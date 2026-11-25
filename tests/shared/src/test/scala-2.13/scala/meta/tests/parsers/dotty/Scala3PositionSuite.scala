package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.tests.parsers.BasePositionSuite

class Scala3PositionSuite extends BasePositionSuite {

  private implicit val dialect: Dialect = dialects.Scala3

  checkPositions[Type]("A & B")
  checkPositions[Type]("A | B")
  checkPositions[Type](
    "[X] =>> (X, X)",
    """|Type.ParamClause [X]
       |Type.ParamClause [X@@] =>> (X, X)
       |Type.Bounds [X@@] =>> (X, X)
       |Type.Tuple (X, X)
       |""".stripMargin,
  )
  checkPositions[Type](
    "[_] =>> Unit",
    """|Type.ParamClause [_]
       |Type.ParamClause [_@@] =>> Unit
       |Type.Bounds [_@@] =>> Unit
       |""".stripMargin,
  )
  checkPositions[Stat]("inline def f = 1")
  checkPositions[Stat](
    "open trait a",
    """|Type.ParamClause open trait a@@
       |Ctor.Primary open trait a@@
       |Template open trait a@@
       |Template.Body open trait a@@
       |""".stripMargin,
  )

  checkPositions[Stat](
    "extension [A, B](i: A)(using a: F[A], G[B]) def isZero = i == 0",
    """|Member.ParamClauseGroup [A, B](i: A)(using a: F[A], G[B])
       |Type.ParamClause [A, B]
       |Type.ParamClause extension [A@@, B](i: A)(using a: F[A], G[B]) def isZero = i == 0
       |Type.Bounds extension [A@@, B](i: A)(using a: F[A], G[B]) def isZero = i == 0
       |Type.ParamClause extension [A, B@@](i: A)(using a: F[A], G[B]) def isZero = i == 0
       |Type.Bounds extension [A, B@@](i: A)(using a: F[A], G[B]) def isZero = i == 0
       |Term.ParamClause (i: A)
       |Term.ParamClause (using a: F[A], G[B])
       |Term.Param a: F[A]
       |Type.Apply F[A]
       |Type.ArgClause [A]
       |Term.Param G[B]
       |Type.Apply G[B]
       |Type.ArgClause [B]
       |Defn.Def def isZero = i == 0
       |Term.ApplyInfix i == 0
       |Type.ArgClause extension [A, B](i: A)(using a: F[A], G[B]) def isZero = i == @@0
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|extension (i: A) {
       |  def isZero = i == 0
       |  def isOne = i == 1
       |}""".stripMargin,
    """|Member.ParamClauseGroup (i: A)
       |Type.ParamClause extension @@(i: A) {
       |Term.ParamClause (i: A)
       |Term.Block {
       |  def isZero = i == 0
       |  def isOne = i == 1
       |}
       |Defn.Def def isZero = i == 0
       |Term.ApplyInfix i == 0
       |Type.ArgClause   def isZero = i == @@0
       |Defn.Def def isOne = i == 1
       |Term.ApplyInfix i == 1
       |Type.ArgClause   def isOne = i == @@1
       |""".stripMargin,
  )

  // This tests exists to document the symmetry between positions for
  // `Mod.Implicit` (test below) and `Mod.Using` (test above).
  checkPositions[Stat](
    "def foo(implicit a: A, b: B): Unit",
    """|Member.ParamClauseGroup (implicit a: A, b: B)
       |Type.ParamClause def foo@@(implicit a: A, b: B): Unit
       |Term.ParamClause (implicit a: A, b: B)
       |Term.Param a: A
       |Term.Param b: B
       |""".stripMargin,
  )

  checkPositions[Stat](
    "enum Day[T](e: T) extends A with B { case Monday, Tuesday }",
    """|Type.ParamClause [T]
       |Type.ParamClause enum Day[T@@](e: T) extends A with B { case Monday, Tuesday }
       |Type.Bounds enum Day[T@@](e: T) extends A with B { case Monday, Tuesday }
       |Ctor.Primary (e: T)
       |Term.ParamClause (e: T)
       |Template extends A with B { case Monday, Tuesday }
       |Template.Body { case Monday, Tuesday }
       |Defn.RepeatedEnumCase case Monday, Tuesday
       |""".stripMargin,
  )
  checkPositions[Stat](
    "class Day[T](e: T) extends A with B { val Monday = 42 }",
    """|Type.ParamClause [T]
       |Type.ParamClause class Day[T@@](e: T) extends A with B { val Monday = 42 }
       |Type.Bounds class Day[T@@](e: T) extends A with B { val Monday = 42 }
       |Ctor.Primary (e: T)
       |Term.ParamClause (e: T)
       |Template extends A with B { val Monday = 42 }
       |Template.Body { val Monday = 42 }
       |Defn.Val val Monday = 42
       |""".stripMargin,
  )
  checkPositions[Stat](
    "inline given intOrd: Ord[Int] with Eq[Int] with { def f(): Int = 1 }",
    """|Template Ord[Int] with Eq[Int] with { def f(): Int = 1 }
       |Init Ord[Int]
       |Type.Apply Ord[Int]
       |Type.ArgClause [Int]
       |Init Eq[Int]
       |Type.Apply Eq[Int]
       |Type.ArgClause [Int]
       |Template.Body { def f(): Int = 1 }
       |Defn.Def def f(): Int = 1
       |Member.ParamClauseGroup ()
       |Type.ParamClause inline given intOrd: Ord[Int] with Eq[Int] with { def f@@(): Int = 1 }
       |Term.ParamClause ()
       |""".stripMargin,
  )
  checkPositions[Stat](
    """|given intOrd: Ord[Int] with Eq[Int] with
       |  // c1
       |  def f(): Int = 1
       |  // c2
       |""".stripMargin,
    """|<templ>Template Ord[Int] with Eq[Int] with
       |  // c1
       |  def f(): Int = 1
       |  // c2</templ>
       |<inits0>Init Ord[Int]</inits0>
       |<tpe>Type.Apply Ord[Int]</tpe>
       |<argClause>Type.ArgClause [Int]</argClause>
       |<inits1>Init Eq[Int]</inits1>
       |<tpe>Type.Apply Eq[Int]</tpe>
       |<argClause>Type.ArgClause [Int]</argClause>
       |<body>Template.Body def f(): Int = 1
       |  // c2</body>
       |<stats0>Defn.Def def f(): Int = 1</stats0>
       |<paramClauseGroups0>Member.ParamClauseGroup ()</paramClauseGroups0>
       |<tparamClause>Type.ParamClause   def f@@(): Int = 1</tparamClause>
       |<paramClauses0>Term.ParamClause ()</paramClauses0>
       |""".stripMargin,
    showFieldName = true,
  )
  checkPositions[Stat](
    """|object A{
       |  inline given intOrd: Ord[Int]
       |}""".stripMargin,
    """|Template {
       |  inline given intOrd: Ord[Int]
       |}
       |Template.Body {
       |  inline given intOrd: Ord[Int]
       |}
       |Decl.Given inline given intOrd: Ord[Int]
       |Type.Apply Ord[Int]
       |Type.ArgClause [Int]
       |""".stripMargin,
  )
  checkPositions[Stat](
    """|object A{
       |  given intOrd: Ord[Int] = intOrd
       |}""".stripMargin,
    """|Template {
       |  given intOrd: Ord[Int] = intOrd
       |}
       |Template.Body {
       |  given intOrd: Ord[Int] = intOrd
       |}
       |Defn.GivenAlias given intOrd: Ord[Int] = intOrd
       |Type.Apply Ord[Int]
       |Type.ArgClause [Int]
       |""".stripMargin,
  )
  checkPositions[Stat](
    """|object A {
       |  export a.b
       |}""".stripMargin,
    """|Template {
       |  export a.b
       |}
       |Template.Body {
       |  export a.b
       |}
       |Export export a.b
       |Importer a.b
       |""".stripMargin,
  )
  checkPositions[Stat](
    "export A.{ b, c, d, _ }",
    """|Importer A.{ b, c, d, _ }
       |Importee.Clause { b, c, d, _ }
       |""".stripMargin,
  )
  checkPositions[Stat](
    "export a.{given Int}",
    """|Importer a.{given Int}
       |Importee.Clause {given Int}
       |Importee.Given given Int
       |""".stripMargin,
  )
  checkPositions[Stat](
    "import Instances.{ im, given Ordering[?] }",
    """|Importer Instances.{ im, given Ordering[?] }
       |Importee.Clause { im, given Ordering[?] }
       |Importee.Given given Ordering[?]
       |Type.Apply Ordering[?]
       |Type.ArgClause [?]
       |Type.Wildcard ?
       |Type.Bounds import Instances.{ im, given Ordering[?@@] }
       |""".stripMargin,
  )
  checkPositions[Stat](
    "import File.given",
    """|Importer File.given
       |Importee.GivenAll given
       |""".stripMargin,
  )
  checkPositions[Type]("A & B")
  checkPositions[Type]("A | B")
  checkPositions[Stat](
    """|type T = A match {
       |  case Char => String
       |  case Array[t] => t
       |}""".stripMargin,
    """|Type.ParamClause type T @@= A match {
       |Type.Match A match {
       |  case Char => String
       |  case Array[t] => t
       |}
       |Type.CasesBlock {
       |  case Char => String
       |  case Array[t] => t
       |}
       |TypeCase case Char => String
       |TypeCase case Array[t] => t
       |Type.Apply Array[t]
       |Type.ArgClause [t]
       |Type.Bounds type T @@= A match {
       |""".stripMargin,
  )
  checkPositions[Stat](
    """|for case a: TP <- iter if cnd do
       |  echo""".stripMargin,
    """|Term.EnumeratorsBlock case a: TP <- iter if cnd
       |Enumerator.CaseGenerator case a: TP <- iter
       |Pat.Typed a: TP
       |Enumerator.Guard if cnd
       |""".stripMargin,
  )
  checkPositions[Stat](
    "infix def a(param: Int) = param",
    """|Member.ParamClauseGroup (param: Int)
       |Type.ParamClause infix def a@@(param: Int) = param
       |Term.ParamClause (param: Int)
       |""".stripMargin,
  )
  checkPositions[Stat](
    "infix type or[X, Y]",
    """|Type.ParamClause [X, Y]
       |Type.ParamClause infix type or[X@@, Y]
       |Type.Bounds infix type or[X@@, Y]
       |Type.ParamClause infix type or[X, Y@@]
       |Type.Bounds infix type or[X, Y@@]
       |Type.Bounds infix type or[X, Y]@@
       |""".stripMargin,
  )
  checkPositions[Stat](
    "def fn: Unit = inline if cond then truep",
    """|Term.If inline if cond then truep
       |Lit.Unit def fn: Unit = inline if cond then truep@@
       |""".stripMargin,
  )
  checkPositions[Stat](
    """|x match {
       |  case '{ a } => 1
       |}""".stripMargin,
    """|Term.CasesBlock {
       |  case '{ a } => 1
       |}
       |Case case '{ a } => 1
       |Pat.Macro '{ a }
       |Term.QuotedMacroExpr '{ a }
       |Term.Block { a }
       |""".stripMargin,
  )
  checkPositions[Stat](
    """|x match {
       |  case List(xs*) => 1
       |}""".stripMargin,
    """|Term.CasesBlock {
       |  case List(xs*) => 1
       |}
       |Case case List(xs*) => 1
       |Pat.Extract List(xs*)
       |Pat.ArgClause (xs*)
       |Pat.Repeated xs*
       |""".stripMargin,
  )
  checkPositions[Stat](
    "val extractor: (e: Entry, f: Other) => e.Key = extractKey",
    """|Type.Function (e: Entry, f: Other) => e.Key
       |Type.FuncParamClause (e: Entry, f: Other)
       |Type.TypedParam e: Entry
       |Type.TypedParam f: Other
       |Type.Select e.Key
       |""".stripMargin,
  )
  checkPositions[Stat](
    "type F0 = [T] => List[T] ?=> Option[T]",
    """|Type.ParamClause type F0 @@= [T] => List[T] ?=> Option[T]
       |Type.PolyFunction [T] => List[T] ?=> Option[T]
       |Type.ParamClause [T]
       |Type.ParamClause type F0 = [T@@] => List[T] ?=> Option[T]
       |Type.Bounds type F0 = [T@@] => List[T] ?=> Option[T]
       |Type.ContextFunction List[T] ?=> Option[T]
       |Type.FuncParamClause List[T]
       |Type.Apply List[T]
       |Type.ArgClause [T]
       |Type.Apply Option[T]
       |Type.ArgClause [T]
       |Type.Bounds type F0 @@= [T] => List[T] ?=> Option[T]
       |""".stripMargin,
  )
  checkPositions[Stat](
    """|inline def g: Any = inline x match {
       |  case x: String => (x, x)
       |  case x: Double => x
       |}""".stripMargin,
    """|Term.Match inline x match {
       |  case x: String => (x, x)
       |  case x: Double => x
       |}
       |Term.CasesBlock {
       |  case x: String => (x, x)
       |  case x: Double => x
       |}
       |Case case x: String => (x, x)
       |Pat.Typed x: String
       |Term.Tuple (x, x)
       |Case case x: Double => x
       |Pat.Typed x: Double
       |""".stripMargin,
  )
  checkPositions[Stat](
    "class Alpha[T] derives Gamma[T], Beta[T]",
    """|Type.ParamClause [T]
       |Type.ParamClause class Alpha[T@@] derives Gamma[T], Beta[T]
       |Type.Bounds class Alpha[T@@] derives Gamma[T], Beta[T]
       |Ctor.Primary class Alpha[T] @@derives Gamma[T], Beta[T]
       |Template derives Gamma[T], Beta[T]
       |Template.Body class Alpha[T] derives Gamma[T], Beta[T]@@
       |Type.Apply Gamma[T]
       |Type.ArgClause [T]
       |Type.Apply Beta[T]
       |Type.ArgClause [T]
       |""".stripMargin,
  )
  checkPositions[Stat](
    """|object O {
       |  import scala as s
       |  import a.b.C as D
       |  import a.*
       |  import a.{no as _, *}
       |  import A.b.`*`
       |  import a.b.C as _
       |}""".stripMargin,
    """|Template {
       |  import scala as s
       |  import a.b.C as D
       |  import a.*
       |  import a.{no as _, *}
       |  import A.b.`*`
       |  import a.b.C as _
       |}
       |Template.Body {
       |  import scala as s
       |  import a.b.C as D
       |  import a.*
       |  import a.{no as _, *}
       |  import A.b.`*`
       |  import a.b.C as _
       |}
       |Import import scala as s
       |Importer scala as s
       |Importee.Rename scala as s
       |Import import a.b.C as D
       |Importer a.b.C as D
       |Term.Select a.b
       |Importee.Rename C as D
       |Import import a.*
       |Importer a.*
       |Importee.Wildcard *
       |Import import a.{no as _, *}
       |Importer a.{no as _, *}
       |Importee.Clause {no as _, *}
       |Importee.Unimport no as _
       |Importee.Wildcard *
       |Import import A.b.`*`
       |Importer A.b.`*`
       |Term.Select A.b
       |Importee.Name `*`
       |Name.Indeterminate `*`
       |Import import a.b.C as _
       |Importer a.b.C as _
       |Term.Select a.b
       |Importee.Unimport C as _
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|object X:
       |  def a: Int =
       |    42
       |  def b: String =
       |    "b"
       |""".stripMargin,
    """|Template :
       |  def a: Int =
       |    42
       |  def b: String =
       |    "b"
       |Template.Body :
       |  def a: Int =
       |    42
       |  def b: String =
       |    "b"
       |Defn.Def def a: Int =
       |    42
       |Defn.Def def b: String =
       |    "b"
       |Lit.String "b"
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|object b:
       |   def foo =
       |     try foo
       |     catch
       |       case a =>
       |     finally bar
       |""".stripMargin,
    """|Template :
       |   def foo =
       |     try foo
       |     catch
       |       case a =>
       |     finally bar
       |Template.Body :
       |   def foo =
       |     try foo
       |     catch
       |       case a =>
       |     finally bar
       |Defn.Def def foo =
       |     try foo
       |     catch
       |       case a =>
       |     finally bar
       |Term.Try try foo
       |     catch
       |       case a =>
       |     finally bar
       |Term.CasesBlock case a =>
       |Case case a =>
       |Term.Block        case a =>@@
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|object a:
       |   def foo =
       |     try foo
       |     catch
       |       case a =>
       |       case b =>
       |         st1
       |         st2
       |     finally bar
       |""".stripMargin,
    """|Template :
       |   def foo =
       |     try foo
       |     catch
       |       case a =>
       |       case b =>
       |         st1
       |         st2
       |     finally bar
       |Template.Body :
       |   def foo =
       |     try foo
       |     catch
       |       case a =>
       |       case b =>
       |         st1
       |         st2
       |     finally bar
       |Defn.Def def foo =
       |     try foo
       |     catch
       |       case a =>
       |       case b =>
       |         st1
       |         st2
       |     finally bar
       |Term.Try try foo
       |     catch
       |       case a =>
       |       case b =>
       |         st1
       |         st2
       |     finally bar
       |Term.CasesBlock case a =>
       |       case b =>
       |         st1
       |         st2
       |Case case a =>
       |Term.Block        @@case b =>
       |Case case b =>
       |         st1
       |         st2
       |Term.Block st1
       |         st2
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|try 
       |  fx
       |  gx
       |  catch
       |  case aa =>
       |  case bb =>
       |  finally
       |  cc
       |  dd
       |""".stripMargin,
    """|Term.Block fx
       |  gx
       |Term.CasesBlock case aa =>
       |  case bb =>
       |Case case aa =>
       |Term.Block   @@case bb =>
       |Case case bb =>
       |Term.Block   case bb =>@@
       |Term.Block cc
       |  dd
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|object A:
       |  
       |  private given x: X = ???
       |""".stripMargin,
    """|Template :
       |  
       |  private given x: X = ???
       |Template.Body :
       |  
       |  private given x: X = ???
       |Defn.GivenAlias private given x: X = ???
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|val a = foo.fold(
       |   err =>
       |  {
       |    42
       |  })
       |""".stripMargin,
    """|Term.Apply foo.fold(
       |   err =>
       |  {
       |    42
       |  })
       |Term.Select foo.fold
       |Term.ArgClause (
       |   err =>
       |  {
       |    42
       |  })
       |Term.Function err =>
       |  {
       |    42
       |  }
       |Term.ParamClause err
       |Term.Param err
       |Term.Block {
       |    42
       |  }
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def a: Unit = 
       |  {
       |    val x = (z: String) =>
       |      fx
       |      gx}
       |""".stripMargin,
    """|Term.Block {
       |    val x = (z: String) =>
       |      fx
       |      gx}
       |Defn.Val val x = (z: String) =>
       |      fx
       |      gx
       |Term.Function (z: String) =>
       |      fx
       |      gx
       |Term.ParamClause (z: String)
       |Term.Block fx
       |      gx
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|foo(
       |  s => 
       |    fx
       |    gx(s),
       |  "yo"
       |)
       |""".stripMargin,
    """|Term.ArgClause (
       |  s => 
       |    fx
       |    gx(s),
       |  "yo"
       |)
       |Term.Function s => 
       |    fx
       |    gx(s)
       |Term.ParamClause s
       |Term.Param s
       |Term.Block fx
       |    gx(s)
       |Term.Apply gx(s)
       |Term.ArgClause (s)
       |Lit.String "yo"
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|foo(
       |  s =>
       |    fx
       |    gx(s)
       |    /* c1 */,
       |  "yo"
       |)
       |""".stripMargin,
    """|Term.ArgClause (
       |  s =>
       |    fx
       |    gx(s)
       |    /* c1 */,
       |  "yo"
       |)
       |Term.Function s =>
       |    fx
       |    gx(s)
       |    /* c1 */
       |Term.ParamClause s
       |Term.Param s
       |Term.Block fx
       |    gx(s)
       |    /* c1 */
       |Term.Apply gx(s)
       |Term.ArgClause (s)
       |Lit.String "yo"
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|inline def encodeFlat =
       |    putInt
       |    inline erasedValue match
       |      case _: Enum      => xs
       |      case _: Singleton => xs
       |    buf
       |""".stripMargin,
    """|Term.Block putInt
       |    inline erasedValue match
       |      case _: Enum      => xs
       |      case _: Singleton => xs
       |    buf
       |Term.Match inline erasedValue match
       |      case _: Enum      => xs
       |      case _: Singleton => xs
       |Term.CasesBlock case _: Enum      => xs
       |      case _: Singleton => xs
       |Case case _: Enum      => xs
       |Pat.Typed _: Enum
       |Case case _: Singleton => xs
       |Pat.Typed _: Singleton
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|val f: Int => Int = (x: Int) => {
       |  x * x
       |}
       |""".stripMargin,
    """|Type.Function Int => Int
       |Type.FuncParamClause Int
       |Term.Function (x: Int) => {
       |  x * x
       |}
       |Term.ParamClause (x: Int)
       |Term.Block {
       |  x * x
       |}
       |Term.ApplyInfix x * x
       |Type.ArgClause   x * @@x
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|(using _: C) => ???
       |""".stripMargin,
    """|Term.ParamClause (using _: C)
       |Term.Param using _: C
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|object x:
       |  if cond then
       |    gx
       |    fx
       |""".stripMargin,
    """|Template :
       |  if cond then
       |    gx
       |    fx
       |Template.Body :
       |  if cond then
       |    gx
       |    fx
       |Term.If if cond then
       |    gx
       |    fx
       |Term.Block gx
       |    fx
       |Lit.Unit     fx@@
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|object x:
       |  val a = 
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |      // c2
       |""".stripMargin,
    """|Template :
       |  val a = 
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |      // c2
       |Template.Body :
       |  val a = 
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |      // c2
       |Defn.Val val a = 
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |      // c2
       |Term.If if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |      // c2
       |Term.Block fx
       |      // c1
       |Term.Block gx
       |      // c2
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|object x:
       |  val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2
       |""".stripMargin,
    """|Template :
       |  val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2
       |Template.Body :
       |  val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2
       |Defn.Val val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2
       |Term.Block if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2
       |Term.If if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |Term.Block fx
       |      // c1
       |""".stripMargin,
  )

  // do not add LF to end of input
  // this test checks `..$comment$EOF` case
  checkPositions[Stat](
    """|object y:
       |  val a = 
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |      // c2""".stripMargin,
    """|Template :
       |  val a = 
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |      // c2
       |Template.Body :
       |  val a = 
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |      // c2
       |Defn.Val val a = 
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |      // c2
       |Term.If if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |      // c2
       |Term.Block fx
       |      // c1
       |Term.Block gx
       |      // c2
       |""".stripMargin,
  )

  // do not add LF to end of input
  // this test checks `..$comment$EOF` case
  checkPositions[Stat](
    """|object y:
       |  val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2""".stripMargin,
    """|Template :
       |  val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2
       |Template.Body :
       |  val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2
       |Defn.Val val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2
       |Term.Block if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2
       |Term.If if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |Term.Block fx
       |      // c1
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|object y {
       |  val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2
       |}""".stripMargin,
    """|Template {
       |  val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2
       |}
       |Template.Body {
       |  val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2
       |}
       |Defn.Val val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2
       |Term.Block if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    // c2
       |Term.If if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |Term.Block fx
       |      // c1
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|object y {
       |  val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    /* c2 */}""".stripMargin,
    """|Template {
       |  val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    /* c2 */}
       |Template.Body {
       |  val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    /* c2 */}
       |Defn.Val val a =
       |    if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    /* c2 */
       |Term.Block if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |    /* c2 */
       |Term.If if cond then
       |      fx
       |      // c1
       |    else
       |      gx
       |Term.Block fx
       |      // c1
       |""".stripMargin,
  )

  // do not add LF to end of input
  // this test checks `..$ident$EOF` case
  checkPositions[Stat](
    """|object ValueTypes:
       |    case object Private
       |        extends ClassificationValue
       |        with ConstantText""".stripMargin,
    """|Template :
       |    case object Private
       |        extends ClassificationValue
       |        with ConstantText
       |Template.Body :
       |    case object Private
       |        extends ClassificationValue
       |        with ConstantText
       |Defn.Object case object Private
       |        extends ClassificationValue
       |        with ConstantText
       |Template extends ClassificationValue
       |        with ConstantText
       |Template.Body         with ConstantText@@
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|val (x, y) = (foo, bar) match
       |  case (false, false) => (baz, qux) // c1""".stripMargin,
    """|Pat.Tuple (x, y)
       |Term.Match (foo, bar) match
       |  case (false, false) => (baz, qux)
       |Term.Tuple (foo, bar)
       |Term.CasesBlock case (false, false) => (baz, qux)
       |Case case (false, false) => (baz, qux)
       |Pat.Tuple (false, false)
       |Term.Tuple (baz, qux)
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|val (x, y) = (foo, bar) match
       |  case (false, false) => (baz, qux) // c1
       |""".stripMargin,
    """|Pat.Tuple (x, y)
       |Term.Match (foo, bar) match
       |  case (false, false) => (baz, qux)
       |Term.Tuple (foo, bar)
       |Term.CasesBlock case (false, false) => (baz, qux)
       |Case case (false, false) => (baz, qux)
       |Pat.Tuple (false, false)
       |Term.Tuple (baz, qux)
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|val (x, y) = (foo, bar) match
       |  case (true, false) => (baz, qux) // c1
       |  case (false, true) => (baz, qux) // c2
       |""".stripMargin,
    """|Pat.Tuple (x, y)
       |Term.Match (foo, bar) match
       |  case (true, false) => (baz, qux) // c1
       |  case (false, true) => (baz, qux)
       |Term.Tuple (foo, bar)
       |Term.CasesBlock case (true, false) => (baz, qux) // c1
       |  case (false, true) => (baz, qux)
       |Case case (true, false) => (baz, qux)
       |Pat.Tuple (true, false)
       |Term.Tuple (baz, qux)
       |Case case (false, true) => (baz, qux)
       |Pat.Tuple (false, true)
       |Term.Tuple (baz, qux)
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|val (x, y) = (foo, bar) match
       |  case (true, false) => (baz, qux) // c1
       |  case (false, true) => (baz, qux) // c2""".stripMargin,
    """|Pat.Tuple (x, y)
       |Term.Match (foo, bar) match
       |  case (true, false) => (baz, qux) // c1
       |  case (false, true) => (baz, qux)
       |Term.Tuple (foo, bar)
       |Term.CasesBlock case (true, false) => (baz, qux) // c1
       |  case (false, true) => (baz, qux)
       |Case case (true, false) => (baz, qux)
       |Pat.Tuple (true, false)
       |Term.Tuple (baz, qux)
       |Case case (false, true) => (baz, qux)
       |Pat.Tuple (false, true)
       |Term.Tuple (baz, qux)
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|(foo, bar) match
       |  case (true, false) => (baz, qux) // c1
       |  case (false, true) => (baz, qux) // c2""".stripMargin,
    """|Term.Tuple (foo, bar)
       |Term.CasesBlock case (true, false) => (baz, qux) // c1
       |  case (false, true) => (baz, qux)
       |Case case (true, false) => (baz, qux)
       |Pat.Tuple (true, false)
       |Term.Tuple (baz, qux)
       |Case case (false, true) => (baz, qux)
       |Pat.Tuple (false, true)
       |Term.Tuple (baz, qux)
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|(foo, bar) match
       |case (true, false) => (baz, qux) // c1
       |case (false, true) => (baz, qux) // c2""".stripMargin,
    """|Term.Tuple (foo, bar)
       |Term.CasesBlock case (true, false) => (baz, qux) // c1
       |case (false, true) => (baz, qux)
       |Case case (true, false) => (baz, qux)
       |Pat.Tuple (true, false)
       |Term.Tuple (baz, qux)
       |Case case (false, true) => (baz, qux)
       |Pat.Tuple (false, true)
       |Term.Tuple (baz, qux)
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|(foo, bar) match
       |case (true, false) => (baz, qux) // c1
       |case (false, true) => (baz, qux) // c2
       |""".stripMargin,
    """|Term.Tuple (foo, bar)
       |Term.CasesBlock case (true, false) => (baz, qux) // c1
       |case (false, true) => (baz, qux)
       |Case case (true, false) => (baz, qux)
       |Pat.Tuple (true, false)
       |Term.Tuple (baz, qux)
       |Case case (false, true) => (baz, qux)
       |Pat.Tuple (false, true)
       |Term.Tuple (baz, qux)
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|val x: (C { type U = T } { type T = String }) # U
       |""".stripMargin,
    """|Type.Project (C { type U = T } { type T = String }) # U
       |Type.Refine C { type U = T } { type T = String }
       |Type.Refine C { type U = T }
       |Stat.Block { type U = T }
       |Defn.Type type U = T
       |Type.ParamClause val x: (C { type U @@= T } { type T = String }) # U
       |Type.Bounds val x: (C { type U @@= T } { type T = String }) # U
       |Stat.Block { type T = String }
       |Defn.Type type T = String
       |Type.ParamClause val x: (C { type U = T } { type T @@= String }) # U
       |Type.Bounds val x: (C { type U = T } { type T @@= String }) # U
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|type A = AnyRef with
       |  type T>: Null
       |""".stripMargin,
    """|Type.ParamClause type A @@= AnyRef with
       |Type.Refine AnyRef with
       |  type T>: Null
       |Stat.Block type T>: Null
       |Decl.Type type T>: Null
       |Type.ParamClause   type T@@>: Null
       |Type.Bounds >: Null
       |Type.Bounds type A @@= AnyRef with
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|type A = AnyRef:
       |  type T>: Null
       |""".stripMargin,
    """|Type.ParamClause type A @@= AnyRef:
       |Type.Refine AnyRef:
       |  type T>: Null
       |Stat.Block :
       |  type T>: Null
       |Decl.Type type T>: Null
       |Type.ParamClause   type T@@>: Null
       |Type.Bounds >: Null
       |Type.Bounds type A @@= AnyRef:
       |""".stripMargin,
  )

  checkPositions[Type](
    """|(x: X, y: Y) => Z
       |""".stripMargin,
    """|Type.FuncParamClause (x: X, y: Y)
       |Type.TypedParam x: X
       |Type.TypedParam y: Y
       |""".stripMargin,
  )

  checkPositions[Type](
    """|(X*, => Y*) => Z
       |""".stripMargin,
    """|Type.FuncParamClause (X*, => Y*)
       |Type.Repeated X*
       |Type.ByName => Y*
       |Type.Repeated Y*
       |""".stripMargin,
  )

  // #3219
  checkPositions[Stat](
    "(10) + 1 toInt",
    """|Term.ApplyInfix (10) + 1
       |Type.ArgClause (10) + @@1 toInt
       |""".stripMargin,
  )
  checkPositions[Stat](
    """|if (10) + 1 == 11 then
       |  fx
       |""".stripMargin,
    """|Term.ApplyInfix (10) + 1 == 11
       |Term.ApplyInfix (10) + 1
       |Type.ArgClause if (10) + @@1 == 11 then
       |Type.ArgClause if (10) + 1 == @@11 then
       |Lit.Unit @@
       |""".stripMargin,
  )

  // #3319
  checkPositions[Stat](
    """|val foo =
       |  bar foreach:
       |    baz
       |""".stripMargin,
    """|Term.ApplyInfix bar foreach:
       |    baz
       |Type.ArgClause   bar foreach@@:
       |Term.Block :
       |    baz
       |""".stripMargin,
  )
  checkPositions[Stat](
    """|val foo =
       |  bar foreach: baz =>
       |    println(baz)
       |""".stripMargin,
    """|Term.ApplyInfix bar foreach: baz =>
       |    println(baz)
       |Type.ArgClause   bar foreach@@: baz =>
       |Term.Block : baz =>
       |    println(baz)
       |Term.Function baz =>
       |    println(baz)
       |Term.ParamClause baz
       |Term.Param baz
       |Term.Apply println(baz)
       |Term.ArgClause (baz)
       |""".stripMargin,
  )

  // #3689 1
  checkPositions[Term](
    """|{
       |  val a = 1 // comment
       |}
       |""".stripMargin,
    """|Defn.Val val a = 1
       |""".stripMargin,
  )

  // #3689 2
  checkPositions[Term](
    """|{
       |  val b =
       |    1 // comment
       |}
       |""".stripMargin,
    """|Defn.Val val b =
       |    1
       |""".stripMargin,
  )

  // #3689 2.1
  checkPositions[Term](
    """|{
       |  val b = {
       |    1 // comment
       |  }
       |}
       |""".stripMargin,
    """|Defn.Val val b = {
       |    1 // comment
       |  }
       |Term.Block {
       |    1 // comment
       |  }
       |""".stripMargin,
  )

  // #3689 3
  checkPositions[Term](
    """|{
       |  val b = // comment1
       |    1 // comment2
       |}
       |""".stripMargin,
    """|Defn.Val val b = // comment1
       |    1
       |""".stripMargin,
  )

  // #3689 3.1
  checkPositions[Term](
    """|{
       |  val b = /* comment1 */ {
       |    1 // comment2
       |  }
       |}
       |""".stripMargin,
    """|Defn.Val val b = /* comment1 */ {
       |    1 // comment2
       |  }
       |Term.Block {
       |    1 // comment2
       |  }
       |""".stripMargin,
  )

  // #3689 4
  checkPositions[Term](
    """|{
       |  val b =
       |    // comment1
       |    1 // comment2
       |}
       |""".stripMargin,
    """|Defn.Val val b =
       |    // comment1
       |    1
       |""".stripMargin,
  )

  // #3689 4.1
  checkPositions[Term](
    """|{
       |  val b = {
       |    // comment1
       |    1 // comment2
       |  }
       |}
       |""".stripMargin,
    """|Defn.Val val b = {
       |    // comment1
       |    1 // comment2
       |  }
       |Term.Block {
       |    // comment1
       |    1 // comment2
       |  }
       |""".stripMargin,
  )

  // #3689 5
  checkPositions[Term](
    """|{
       |  val b = // comment
       |    1
       |}
       |""".stripMargin,
    """|Defn.Val val b = // comment
       |    1
       |""".stripMargin,
  )

  // #3689 5.1
  checkPositions[Term](
    """|{
       |  val b = /* comment */ {
       |    1
       |  }
       |}
       |""".stripMargin,
    """|Defn.Val val b = /* comment */ {
       |    1
       |  }
       |Term.Block {
       |    1
       |  }
       |""".stripMargin,
  )

  // #3689 6
  checkPositions[Term](
    """|{
       |  val b =
       |    // comment
       |    1
       |}
       |""".stripMargin,
    """|Defn.Val val b =
       |    // comment
       |    1
       |""".stripMargin,
  )

  // #3689 6.1
  checkPositions[Term](
    """|{
       |  val b = {
       |    // comment
       |    1
       |  }
       |}
       |""".stripMargin,
    """|Defn.Val val b = {
       |    // comment
       |    1
       |  }
       |Term.Block {
       |    // comment
       |    1
       |  }
       |""".stripMargin,
  )

  // #3689 7
  checkPositions[Term](
    """|{
       |  val b =
       |    1
       |    // comment
       |}
       |""".stripMargin,
    """|Defn.Val val b =
       |    1
       |    // comment
       |Term.Block 1
       |    // comment
       |""".stripMargin,
  )

  // #3689 7.1
  checkPositions[Term](
    """|{
       |  val b = {
       |    1
       |    // comment
       |  }
       |}
       |""".stripMargin,
    """|Defn.Val val b = {
       |    1
       |    // comment
       |  }
       |Term.Block {
       |    1
       |    // comment
       |  }
       |""".stripMargin,
  )

  // #3689 8
  checkPositions[Term](
    """|{
       |  val b = // comment1
       |    1
       |    // comment2
       |}
       |""".stripMargin,
    """|Defn.Val val b = // comment1
       |    1
       |    // comment2
       |Term.Block 1
       |    // comment2
       |""".stripMargin,
  )

  // #3689 8.1
  checkPositions[Term](
    """|{
       |  val b = /* comment1 */ {
       |    1
       |    // comment2
       |  }
       |}
       |""".stripMargin,
    """|Defn.Val val b = /* comment1 */ {
       |    1
       |    // comment2
       |  }
       |Term.Block {
       |    1
       |    // comment2
       |  }
       |""".stripMargin,
  )

  // #3689 9
  checkPositions[Term](
    """|{
       |  val b =
       |    // comment1
       |    1
       |    // comment2
       |}
       |""".stripMargin,
    """|Defn.Val val b =
       |    // comment1
       |    1
       |    // comment2
       |Term.Block 1
       |    // comment2
       |""".stripMargin,
  )

  // #3689 9.1
  checkPositions[Term](
    """|{
       |  val b = {
       |    // comment1
       |    1
       |    // comment2
       |  }
       |}
       |""".stripMargin,
    """|Defn.Val val b = {
       |    // comment1
       |    1
       |    // comment2
       |  }
       |Term.Block {
       |    // comment1
       |    1
       |    // comment2
       |  }
       |""".stripMargin,
  )

  checkPositions[Term](
    """|if a then
       |  if aa then
       |    aaa
       |    // c1
       |  if aa then
       |    aaa
       |""".stripMargin,
    """|<thenp>Term.Block if aa then
       |    aaa
       |    // c1
       |  if aa then
       |    aaa</thenp>
       |<stats0>Term.If if aa then
       |    aaa
       |    // c1</stats0>
       |<thenp>Term.Block aaa
       |    // c1</thenp>
       |<elsep>Lit.Unit   @@if aa then</elsep>
       |<stats1>Term.If if aa then
       |    aaa</stats1>
       |<elsep>Lit.Unit     aaa@@</elsep>
       |<elsep>Lit.Unit @@</elsep>
       |""".stripMargin,
    """|BOF [0..0)
       |KwIf [0..2)
       |Ident(a) [3..4)
       |KwThen [5..9)
       |Indentation.Indent [9..9)
       |KwIf [12..14)
       |Ident(aa) [15..17)
       |KwThen [18..22)
       |Indentation.Indent [22..22)
       |Ident(aaa) [27..30)
       |Indentation.Outdent [40..40)
       |KwIf [43..45)
       |Ident(aa) [46..48)
       |KwThen [49..53)
       |Indentation.Indent [53..53)
       |Ident(aaa) [58..61)
       |Indentation.Outdent [61..61)
       |Indentation.Outdent [61..61)
       |EOF [62..62)
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Term](
    """|if (a)
       |  b.c()
       |    // c1
       |    // c2
       |""".stripMargin,
    """|<thenp>Term.Block b.c()
       |    // c1
       |    // c2</thenp>
       |<stats0>Term.Apply b.c()</stats0>
       |<fun>Term.Select b.c</fun>
       |<argClause>Term.ArgClause ()</argClause>
       |<elsep>Lit.Unit @@</elsep>
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Stat](
    """|def foo =
       |  if (a)
       |    b.c()
       |      // c1
       |      // c2
       |  d.e
       |""".stripMargin,
    """|<body>Term.Block if (a)
       |    b.c()
       |      // c1
       |      // c2
       |  d.e</body>
       |<stats0>Term.If if (a)
       |    b.c()
       |      // c1
       |      // c2</stats0>
       |<thenp>Term.Block b.c()
       |      // c1
       |      // c2</thenp>
       |<stats0>Term.Apply b.c()</stats0>
       |<fun>Term.Select b.c</fun>
       |<argClause>Term.ArgClause ()</argClause>
       |<elsep>Lit.Unit   @@d.e</elsep>
       |<stats1>Term.Select d.e</stats1>
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(foo) [4..7)
       |Equals [8..9)
       |Indentation.Indent [9..9)
       |KwIf [12..14)
       |LeftParen [15..16)
       |Ident(a) [16..17)
       |RightParen [17..18)
       |Indentation.Indent [18..18)
       |Ident(b) [23..24)
       |Dot [24..25)
       |Ident(c) [25..26)
       |LeftParen [26..27)
       |RightParen [27..28)
       |Indentation.Outdent [52..52)
       |Ident(d) [55..56)
       |Dot [56..57)
       |Ident(e) [57..58)
       |Indentation.Outdent [58..58)
       |EOF [59..59)
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Term](
    """|{
       |  freezing /*
       |    c1 */ // c2
       |  |
       |  boiling
       |}
       |""".stripMargin,
    """|<stats0>Term.ApplyInfix freezing /*
       |    c1 */ // c2
       |  |
       |  boiling</stats0>
       |<targClause>Type.ArgClause   @@boiling</targClause>
       |""".stripMargin,
    """|BOF [0..0)
       |LeftBrace [0..1)
       |Ident(freezing) [4..12)
       |InfixLF [31..32)
       |Ident(|) [34..35)
       |LF [35..36)
       |Ident(boiling) [38..45)
       |LF [45..46)
       |RightBrace [46..47)
       |EOF [48..48)
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Term](
    """|{
       |  freezing
       |  // c1
       |  /*
       |    c1
       |  */ |
       |  boiling
       |}
       |""".stripMargin,
    """|<stats0>Term.ApplyInfix freezing
       |  // c1
       |  /*
       |    c1
       |  */ |
       |  boiling</stats0>
       |<targClause>Type.ArgClause   @@boiling</targClause>
       |""".stripMargin,
    """|BOF [0..0)
       |LeftBrace [0..1)
       |Ident(freezing) [4..12)
       |InfixLF [23..37)
       |Ident(|) [38..39)
       |LF [39..40)
       |Ident(boiling) [42..49)
       |LF [49..50)
       |RightBrace [50..51)
       |EOF [52..52)
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Term](
    """|{
       |  freezing
       |  // c1
       |  /*
       |    c1
       |  */
       |  |
       |  boiling
       |}
       |""".stripMargin,
    """|<stats0>Term.ApplyInfix freezing
       |  // c1
       |  /*
       |    c1
       |  */
       |  |
       |  boiling</stats0>
       |<targClause>Type.ArgClause   @@boiling</targClause>
       |""".stripMargin,
    """|BOF [0..0)
       |LeftBrace [0..1)
       |Ident(freezing) [4..12)
       |InfixLF [37..38)
       |Ident(|) [40..41)
       |LF [41..42)
       |Ident(boiling) [44..51)
       |LF [51..52)
       |RightBrace [52..53)
       |EOF [54..54)
       |""".stripMargin,
    showFieldName = true,
  )

  // comment at end of match but not before case outdent
  checkPositions[Stat](
    """|object a:
       |  bar2 match
       |     case foo1: Foo1 => foo1
       |     case foo2: Foo2 =>
       |       foo2 += " = "
       |     // foo2
       |  end match
       |""".stripMargin,
    """|<templ>Template :
       |  bar2 match
       |     case foo1: Foo1 => foo1
       |     case foo2: Foo2 =>
       |       foo2 += " = "
       |     // foo2
       |  end match</templ>
       |<body>Template.Body :
       |  bar2 match
       |     case foo1: Foo1 => foo1
       |     case foo2: Foo2 =>
       |       foo2 += " = "
       |     // foo2
       |  end match</body>
       |<stats0>Term.Match bar2 match
       |     case foo1: Foo1 => foo1
       |     case foo2: Foo2 =>
       |       foo2 += " = "
       |     // foo2</stats0>
       |<casesBlock>Term.CasesBlock case foo1: Foo1 => foo1
       |     case foo2: Foo2 =>
       |       foo2 += " = "
       |     // foo2</casesBlock>
       |<cases0>Case case foo1: Foo1 => foo1</cases0>
       |<pat>Pat.Typed foo1: Foo1</pat>
       |<cases1>Case case foo2: Foo2 =>
       |       foo2 += " = "</cases1>
       |<pat>Pat.Typed foo2: Foo2</pat>
       |<body>Term.ApplyInfix foo2 += " = "</body>
       |<targClause>Type.ArgClause        foo2 += @@" = "</targClause>
       |<values0>Lit.String " = "</values0>
       |<stats1>Term.EndMarker end match</stats1>
       |""".stripMargin,
    """|BOF [0..0)
       |KwObject [0..6)
       |Ident(a) [7..8)
       |Colon [8..9)
       |Indentation.Indent [9..9)
       |Ident(bar2) [12..16)
       |KwMatch [17..22)
       |Indentation.Indent [22..22)
       |KwCase [28..32)
       |Ident(foo1) [33..37)
       |Colon [37..38)
       |Ident(Foo1) [39..43)
       |RightArrow [44..46)
       |Ident(foo1) [47..51)
       |LF [51..52)
       |KwCase [57..61)
       |Ident(foo2) [62..66)
       |Colon [66..67)
       |Ident(Foo2) [68..72)
       |RightArrow [73..75)
       |Indentation.Indent [75..75)
       |Ident(foo2) [83..87)
       |Ident(+=) [88..90)
       |Constant.String( = ) [91..96)
       |Indentation.Outdent [96..96)
       |Indentation.Outdent [109..109)
       |Ident(end) [112..115)
       |KwMatch [116..121)
       |Indentation.Outdent [121..121)
       |EOF [122..122)
       |""".stripMargin,
    showFieldName = true,
  )

  // comment at end of match and also at end of case
  checkPositions[Stat](
    """|object a:
       |  bar2 match
       |     case foo1: Foo1 => foo1
       |     case foo2: Foo2 =>
       |       foo2 += " = "
       |       // foo2
       |  end match
       |""".stripMargin,
    """|<templ>Template :
       |  bar2 match
       |     case foo1: Foo1 => foo1
       |     case foo2: Foo2 =>
       |       foo2 += " = "
       |       // foo2
       |  end match</templ>
       |<body>Template.Body :
       |  bar2 match
       |     case foo1: Foo1 => foo1
       |     case foo2: Foo2 =>
       |       foo2 += " = "
       |       // foo2
       |  end match</body>
       |<stats0>Term.Match bar2 match
       |     case foo1: Foo1 => foo1
       |     case foo2: Foo2 =>
       |       foo2 += " = "
       |       // foo2</stats0>
       |<casesBlock>Term.CasesBlock case foo1: Foo1 => foo1
       |     case foo2: Foo2 =>
       |       foo2 += " = "
       |       // foo2</casesBlock>
       |<cases0>Case case foo1: Foo1 => foo1</cases0>
       |<pat>Pat.Typed foo1: Foo1</pat>
       |<cases1>Case case foo2: Foo2 =>
       |       foo2 += " = "
       |       // foo2</cases1>
       |<pat>Pat.Typed foo2: Foo2</pat>
       |<body>Term.Block foo2 += " = "
       |       // foo2</body>
       |<stats0>Term.ApplyInfix foo2 += " = "</stats0>
       |<targClause>Type.ArgClause        foo2 += @@" = "</targClause>
       |<values0>Lit.String " = "</values0>
       |<stats1>Term.EndMarker end match</stats1>
       |""".stripMargin,
    """|BOF [0..0)
       |KwObject [0..6)
       |Ident(a) [7..8)
       |Colon [8..9)
       |Indentation.Indent [9..9)
       |Ident(bar2) [12..16)
       |KwMatch [17..22)
       |Indentation.Indent [22..22)
       |KwCase [28..32)
       |Ident(foo1) [33..37)
       |Colon [37..38)
       |Ident(Foo1) [39..43)
       |RightArrow [44..46)
       |Ident(foo1) [47..51)
       |LF [51..52)
       |KwCase [57..61)
       |Ident(foo2) [62..66)
       |Colon [66..67)
       |Ident(Foo2) [68..72)
       |RightArrow [73..75)
       |Indentation.Indent [75..75)
       |Ident(foo2) [83..87)
       |Ident(+=) [88..90)
       |Constant.String( = ) [91..96)
       |Indentation.Outdent [111..111)
       |Indentation.Outdent [111..111)
       |Ident(end) [114..117)
       |KwMatch [118..123)
       |Indentation.Outdent [123..123)
       |EOF [124..124)
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Source](
    """|package foo
       |// c1
       |
       |package bar:
       |  // c2
       |  class Baz
       |  // c3
       |""".stripMargin,
    """|<stats0>Pkg package foo
       |// c1
       |
       |package bar:
       |  // c2
       |  class Baz
       |  // c3</stats0>
       |<body>Pkg.Body package bar:
       |  // c2
       |  class Baz
       |  // c3</body>
       |<stats0>Pkg package bar:
       |  // c2
       |  class Baz
       |  // c3</stats0>
       |<body>Pkg.Body :
       |  // c2
       |  class Baz
       |  // c3</body>
       |<stats0>Defn.Class class Baz</stats0>
       |<tparamClause>Type.ParamClause   // c3@@</tparamClause>
       |<ctor>Ctor.Primary   class Baz@@</ctor>
       |<templ>Template   // c3@@</templ>
       |<body>Template.Body   // c3@@</body>
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Source](
    """|package foo
       |// c1
       |
       |package bar {
       |  // c2
       |  class Baz
       |  // c3
       |}
       |""".stripMargin,
    """|<stats0>Pkg package foo
       |// c1
       |
       |package bar {
       |  // c2
       |  class Baz
       |  // c3
       |}</stats0>
       |<body>Pkg.Body package bar {
       |  // c2
       |  class Baz
       |  // c3
       |}</body>
       |<stats0>Pkg package bar {
       |  // c2
       |  class Baz
       |  // c3
       |}</stats0>
       |<body>Pkg.Body {
       |  // c2
       |  class Baz
       |  // c3
       |}</body>
       |<stats0>Defn.Class class Baz</stats0>
       |<tparamClause>Type.ParamClause   class Baz@@</tparamClause>
       |<ctor>Ctor.Primary   class Baz@@</ctor>
       |<templ>Template   class Baz@@</templ>
       |<body>Template.Body   class Baz@@</body>
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Source](
    """|object a:
       |   try baz
       |   catch case ex: Baz => foo
       |
       |   // comment
       |   val qux = quux
       |""".stripMargin,
    """|<stats0>Defn.Object object a:
       |   try baz
       |   catch case ex: Baz => foo
       |
       |   // comment
       |   val qux = quux</stats0>
       |<templ>Template :
       |   try baz
       |   catch case ex: Baz => foo
       |
       |   // comment
       |   val qux = quux</templ>
       |<body>Template.Body :
       |   try baz
       |   catch case ex: Baz => foo
       |
       |   // comment
       |   val qux = quux</body>
       |<stats0>Term.Try try baz
       |   catch case ex: Baz => foo</stats0>
       |<catchClause>Term.CasesBlock case ex: Baz => foo</catchClause>
       |<cases0>Case case ex: Baz => foo</cases0>
       |<pat>Pat.Typed ex: Baz</pat>
       |<stats1>Defn.Val val qux = quux</stats1>
       |""".stripMargin,
    """|BOF [0..0)
       |KwObject [0..6)
       |Ident(a) [7..8)
       |Colon [8..9)
       |Indentation.Indent [9..9)
       |KwTry [13..16)
       |Ident(baz) [17..20)
       |KwCatch [24..29)
       |Indentation.Indent [30..30)
       |KwCase [30..34)
       |Ident(ex) [35..37)
       |Colon [37..38)
       |Ident(Baz) [39..42)
       |RightArrow [43..45)
       |Ident(foo) [46..49)
       |Indentation.Outdent [50..50)
       |LFLF [50..65)
       |KwVal [68..71)
       |Ident(qux) [72..75)
       |Equals [76..77)
       |Ident(quux) [78..82)
       |Indentation.Outdent [82..82)
       |EOF [83..83)
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Source](
    """|object a {
       |  def foo = try
       |    bar
       |  catch case baz => qux
       |
       |  /** c1 */
       |  def quux = ???
       |}
       |""".stripMargin,
    """|<stats0>Defn.Object object a {
       |  def foo = try
       |    bar
       |  catch case baz => qux
       |
       |  /** c1 */
       |  def quux = ???
       |}</stats0>
       |<templ>Template {
       |  def foo = try
       |    bar
       |  catch case baz => qux
       |
       |  /** c1 */
       |  def quux = ???
       |}</templ>
       |<body>Template.Body {
       |  def foo = try
       |    bar
       |  catch case baz => qux
       |
       |  /** c1 */
       |  def quux = ???
       |}</body>
       |<stats0>Defn.Def def foo = try
       |    bar
       |  catch case baz => qux</stats0>
       |<body>Term.Try try
       |    bar
       |  catch case baz => qux</body>
       |<catchClause>Term.CasesBlock case baz => qux</catchClause>
       |<cases0>Case case baz => qux</cases0>
       |<stats1>Defn.Def def quux = ???</stats1>
       |""".stripMargin,
    """|BOF [0..0)
       |KwObject [0..6)
       |Ident(a) [7..8)
       |LeftBrace [9..10)
       |KwDef [13..16)
       |Ident(foo) [17..20)
       |Equals [21..22)
       |KwTry [23..26)
       |Indentation.Indent [26..26)
       |Ident(bar) [31..34)
       |Indentation.Outdent [34..34)
       |KwCatch [37..42)
       |Indentation.Indent [43..43)
       |KwCase [43..47)
       |Ident(baz) [48..51)
       |RightArrow [52..54)
       |Ident(qux) [55..58)
       |Indentation.Outdent [59..59)
       |LFLF [59..72)
       |KwDef [74..77)
       |Ident(quux) [78..82)
       |Equals [83..84)
       |Ident(???) [85..88)
       |LF [88..89)
       |RightBrace [89..90)
       |EOF [91..91)
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Source](
    """|object a:
       |  foo match {
       |    case null =>
       |      try
       |        val bar = baz
       |      catch case ex => qux
       |        // c1
       |
       |      // c2
       |
       |    // c3
       |  }
       |""".stripMargin,
    """|<stats0>Defn.Object object a:
       |  foo match {
       |    case null =>
       |      try
       |        val bar = baz
       |      catch case ex => qux
       |        // c1
       |
       |      // c2
       |
       |    // c3
       |  }</stats0>
       |<templ>Template :
       |  foo match {
       |    case null =>
       |      try
       |        val bar = baz
       |      catch case ex => qux
       |        // c1
       |
       |      // c2
       |
       |    // c3
       |  }</templ>
       |<body>Template.Body :
       |  foo match {
       |    case null =>
       |      try
       |        val bar = baz
       |      catch case ex => qux
       |        // c1
       |
       |      // c2
       |
       |    // c3
       |  }</body>
       |<stats0>Term.Match foo match {
       |    case null =>
       |      try
       |        val bar = baz
       |      catch case ex => qux
       |        // c1
       |
       |      // c2
       |
       |    // c3
       |  }</stats0>
       |<casesBlock>Term.CasesBlock {
       |    case null =>
       |      try
       |        val bar = baz
       |      catch case ex => qux
       |        // c1
       |
       |      // c2
       |
       |    // c3
       |  }</casesBlock>
       |<cases0>Case case null =>
       |      try
       |        val bar = baz
       |      catch case ex => qux
       |        // c1</cases0>
       |<body>Term.Try try
       |        val bar = baz
       |      catch case ex => qux
       |        // c1</body>
       |<expr>Term.Block val bar = baz</expr>
       |<stats0>Defn.Val val bar = baz</stats0>
       |<catchClause>Term.CasesBlock case ex => qux
       |        // c1</catchClause>
       |<cases0>Case case ex => qux</cases0>
       |""".stripMargin,
    """|BOF [0..0)
       |KwObject [0..6)
       |Ident(a) [7..8)
       |Colon [8..9)
       |Indentation.Indent [9..9)
       |Ident(foo) [12..15)
       |KwMatch [16..21)
       |LeftBrace [22..23)
       |KwCase [28..32)
       |KwNull [33..37)
       |RightArrow [38..40)
       |KwTry [47..50)
       |Indentation.Indent [50..50)
       |KwVal [59..62)
       |Ident(bar) [63..66)
       |Equals [67..68)
       |Ident(baz) [69..72)
       |Indentation.Outdent [72..72)
       |KwCatch [79..84)
       |Indentation.Indent [85..85)
       |KwCase [85..89)
       |Ident(ex) [90..92)
       |RightArrow [93..95)
       |Ident(qux) [96..99)
       |Indentation.Outdent [114..114)
       |RightBrace [140..141)
       |Indentation.Outdent [141..141)
       |EOF [142..142)
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Source](
    """|object a:
       |  foo match {
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4
       |  }
       |""".stripMargin,
    """|<stats0>Defn.Object object a:
       |  foo match {
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4
       |  }</stats0>
       |<templ>Template :
       |  foo match {
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4
       |  }</templ>
       |<body>Template.Body :
       |  foo match {
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4
       |  }</body>
       |<stats0>Term.Match foo match {
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4
       |  }</stats0>
       |<casesBlock>Term.CasesBlock {
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4
       |  }</casesBlock>
       |<cases0>Case case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2</cases0>
       |<body>Term.Try try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2</body>
       |<expr>Term.Block val bar = baz</expr>
       |<stats0>Defn.Val val bar = baz</stats0>
       |<catchClause>Term.CasesBlock case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2</catchClause>
       |<cases0>Case case ex =>
       |          qux
       |
       |          // c1</cases0>
       |<body>Term.Block qux
       |
       |          // c1</body>
       |""".stripMargin,
    """|BOF [0..0)
       |KwObject [0..6)
       |Ident(a) [7..8)
       |Colon [8..9)
       |Indentation.Indent [9..9)
       |Ident(foo) [12..15)
       |KwMatch [16..21)
       |LeftBrace [22..23)
       |KwCase [28..32)
       |KwNull [33..37)
       |RightArrow [38..40)
       |KwTry [47..50)
       |Indentation.Indent [50..50)
       |KwVal [59..62)
       |Ident(bar) [63..66)
       |Equals [67..68)
       |Ident(baz) [69..72)
       |Indentation.Outdent [72..72)
       |KwCatch [79..84)
       |Indentation.Indent [84..84)
       |KwCase [93..97)
       |Ident(ex) [98..100)
       |RightArrow [101..103)
       |Indentation.Indent [103..103)
       |Ident(qux) [114..117)
       |Indentation.Outdent [135..135)
       |Indentation.Outdent [150..150)
       |RightBrace [176..177)
       |Indentation.Outdent [177..177)
       |EOF [178..178)
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Source](
    """|object a {
       |  foo match
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4
       |
       |  // c5
       |}
       |""".stripMargin,
    """|<stats0>Defn.Object object a {
       |  foo match
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4
       |
       |  // c5
       |}</stats0>
       |<templ>Template {
       |  foo match
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4
       |
       |  // c5
       |}</templ>
       |<body>Template.Body {
       |  foo match
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4
       |
       |  // c5
       |}</body>
       |<stats0>Term.Match foo match
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4</stats0>
       |<casesBlock>Term.CasesBlock case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4</casesBlock>
       |<cases0>Case case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3</cases0>
       |<body>Term.Block try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3</body>
       |<stats0>Term.Try try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2</stats0>
       |<expr>Term.Block val bar = baz</expr>
       |<stats0>Defn.Val val bar = baz</stats0>
       |<catchClause>Term.CasesBlock case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2</catchClause>
       |<cases0>Case case ex =>
       |          qux
       |
       |          // c1</cases0>
       |<body>Term.Block qux
       |
       |          // c1</body>
       |""".stripMargin,
    """|BOF [0..0)
       |KwObject [0..6)
       |Ident(a) [7..8)
       |LeftBrace [9..10)
       |Ident(foo) [13..16)
       |KwMatch [17..22)
       |Indentation.Indent [22..22)
       |KwCase [27..31)
       |KwNull [32..36)
       |RightArrow [37..39)
       |Indentation.Indent [39..39)
       |KwTry [46..49)
       |Indentation.Indent [49..49)
       |KwVal [58..61)
       |Ident(bar) [62..65)
       |Equals [66..67)
       |Ident(baz) [68..71)
       |Indentation.Outdent [71..71)
       |KwCatch [78..83)
       |Indentation.Indent [83..83)
       |KwCase [92..96)
       |Ident(ex) [97..99)
       |RightArrow [100..102)
       |Indentation.Indent [102..102)
       |Ident(qux) [113..116)
       |Indentation.Outdent [134..134)
       |Indentation.Outdent [149..149)
       |Indentation.Outdent [162..162)
       |Indentation.Outdent [173..173)
       |RightBrace [182..183)
       |EOF [184..184)
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Source](
    """|object a:
       |  foo match
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4
       |
       |  // c5
       |""".stripMargin,
    """|<stats0>Defn.Object object a:
       |  foo match
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4
       |
       |  // c5</stats0>
       |<templ>Template :
       |  foo match
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4
       |
       |  // c5</templ>
       |<body>Template.Body :
       |  foo match
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4
       |
       |  // c5</body>
       |<stats0>Term.Match foo match
       |    case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4</stats0>
       |<casesBlock>Term.CasesBlock case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3
       |
       |    // c4</casesBlock>
       |<cases0>Case case null =>
       |      try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3</cases0>
       |<body>Term.Block try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2
       |
       |      // c3</body>
       |<stats0>Term.Try try
       |        val bar = baz
       |      catch
       |        case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2</stats0>
       |<expr>Term.Block val bar = baz</expr>
       |<stats0>Defn.Val val bar = baz</stats0>
       |<catchClause>Term.CasesBlock case ex =>
       |          qux
       |
       |          // c1
       |
       |        // c2</catchClause>
       |<cases0>Case case ex =>
       |          qux
       |
       |          // c1</cases0>
       |<body>Term.Block qux
       |
       |          // c1</body>
       |""".stripMargin,
    """|BOF [0..0)
       |KwObject [0..6)
       |Ident(a) [7..8)
       |Colon [8..9)
       |Indentation.Indent [9..9)
       |Ident(foo) [12..15)
       |KwMatch [16..21)
       |Indentation.Indent [21..21)
       |KwCase [26..30)
       |KwNull [31..35)
       |RightArrow [36..38)
       |Indentation.Indent [38..38)
       |KwTry [45..48)
       |Indentation.Indent [48..48)
       |KwVal [57..60)
       |Ident(bar) [61..64)
       |Equals [65..66)
       |Ident(baz) [67..70)
       |Indentation.Outdent [70..70)
       |KwCatch [77..82)
       |Indentation.Indent [82..82)
       |KwCase [91..95)
       |Ident(ex) [96..98)
       |RightArrow [99..101)
       |Indentation.Indent [101..101)
       |Ident(qux) [112..115)
       |Indentation.Outdent [133..133)
       |Indentation.Outdent [148..148)
       |Indentation.Outdent [161..161)
       |Indentation.Outdent [172..172)
       |Indentation.Outdent [180..180)
       |EOF [181..181)
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Stat](
    """|extension (a: Int)
       |
       |
       |    /** */
       |
       |
       |    def double = a * 2
       |
       |    /** */
       |""".stripMargin,
    """|<paramClauseGroup>Member.ParamClauseGroup (a: Int)</paramClauseGroup>
       |<tparamClause>Type.ParamClause extension @@(a: Int)</tparamClause>
       |<paramClauses0>Term.ParamClause (a: Int)</paramClauses0>
       |<body>Term.Block def double = a * 2
       |
       |    /** */</body>
       |<stats0>Defn.Def def double = a * 2</stats0>
       |<body>Term.ApplyInfix a * 2</body>
       |<targClause>Type.ArgClause     def double = a * @@2</targClause>
       |""".stripMargin,
    """|BOF [0..0)
       |Ident(extension) [0..9)
       |LeftParen [10..11)
       |Ident(a) [11..12)
       |Colon [12..13)
       |Ident(Int) [14..17)
       |RightParen [17..18)
       |Indentation.Indent [33..33)
       |KwDef [38..41)
       |Ident(double) [42..48)
       |Equals [49..50)
       |Ident(a) [51..52)
       |Ident(*) [53..54)
       |Constant.Int(2) [55..56)
       |Indentation.Outdent [68..68)
       |EOF [69..69)
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Stat](
    """|try foo
       |catch
       |case _ =>
       |    try bar finally qux
       |    try bar catch baz finally qux
       |    try bar catch case _ => baz finally qux
       |case xyz =>
       |""".stripMargin,
    """|<catchClause>Term.CasesBlock case _ =>
       |    try bar finally qux
       |    try bar catch baz finally qux
       |    try bar catch case _ => baz finally qux
       |case xyz =></catchClause>
       |<cases0>Case case _ =>
       |    try bar finally qux
       |    try bar catch baz finally qux
       |    try bar catch case _ => baz finally qux</cases0>
       |<body>Term.Block try bar finally qux
       |    try bar catch baz finally qux
       |    try bar catch case _ => baz finally qux</body>
       |<stats0>Term.Try try bar finally qux</stats0>
       |<stats1>Term.TryWithHandler try bar catch baz finally qux</stats1>
       |<stats2>Term.Try try bar catch case _ => baz finally qux</stats2>
       |<catchClause>Term.CasesBlock case _ => baz</catchClause>
       |<cases0>Case case _ => baz</cases0>
       |<cases1>Case case xyz =></cases1>
       |<body>Term.Block case xyz =>@@</body>
       |""".stripMargin,
    """|BOF [0..0)
       |KwTry [0..3)
       |Ident(foo) [4..7)
       |KwCatch [8..13)
       |Indentation.Indent [13..13)
       |KwCase [14..18)
       |Underscore [19..20)
       |RightArrow [21..23)
       |Indentation.Indent [23..23)
       |KwTry [28..31)
       |Ident(bar) [32..35)
       |KwFinally [36..43)
       |Ident(qux) [44..47)
       |LF [47..48)
       |KwTry [52..55)
       |Ident(bar) [56..59)
       |KwCatch [60..65)
       |Ident(baz) [66..69)
       |KwFinally [70..77)
       |Ident(qux) [78..81)
       |LF [81..82)
       |KwTry [86..89)
       |Ident(bar) [90..93)
       |KwCatch [94..99)
       |Indentation.Indent [100..100)
       |KwCase [100..104)
       |Underscore [105..106)
       |RightArrow [107..109)
       |Ident(baz) [110..113)
       |Indentation.Outdent [113..113)
       |KwFinally [114..121)
       |Ident(qux) [122..125)
       |Indentation.Outdent [125..125)
       |LF [125..126)
       |KwCase [126..130)
       |Ident(xyz) [131..134)
       |RightArrow [135..137)
       |Indentation.Outdent [137..137)
       |EOF [138..138)
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Stat](
    """|try foo
       |catch
       |case _ =>
       |    try bar finally qux
       |    try bar catch baz finally qux
       |    try bar catch case _ => baz finally qux
       |finally xyz
       |""".stripMargin,
    """|<catchClause>Term.CasesBlock case _ =>
       |    try bar finally qux
       |    try bar catch baz finally qux
       |    try bar catch case _ => baz finally qux</catchClause>
       |<cases0>Case case _ =>
       |    try bar finally qux
       |    try bar catch baz finally qux
       |    try bar catch case _ => baz finally qux</cases0>
       |<body>Term.Block try bar finally qux
       |    try bar catch baz finally qux
       |    try bar catch case _ => baz finally qux</body>
       |<stats0>Term.Try try bar finally qux</stats0>
       |<stats1>Term.TryWithHandler try bar catch baz finally qux</stats1>
       |<stats2>Term.Try try bar catch case _ => baz finally qux</stats2>
       |<catchClause>Term.CasesBlock case _ => baz</catchClause>
       |<cases0>Case case _ => baz</cases0>
       |""".stripMargin,
    """|BOF [0..0)
       |KwTry [0..3)
       |Ident(foo) [4..7)
       |KwCatch [8..13)
       |Indentation.Indent [13..13)
       |KwCase [14..18)
       |Underscore [19..20)
       |RightArrow [21..23)
       |Indentation.Indent [23..23)
       |KwTry [28..31)
       |Ident(bar) [32..35)
       |KwFinally [36..43)
       |Ident(qux) [44..47)
       |LF [47..48)
       |KwTry [52..55)
       |Ident(bar) [56..59)
       |KwCatch [60..65)
       |Ident(baz) [66..69)
       |KwFinally [70..77)
       |Ident(qux) [78..81)
       |LF [81..82)
       |KwTry [86..89)
       |Ident(bar) [90..93)
       |KwCatch [94..99)
       |Indentation.Indent [100..100)
       |KwCase [100..104)
       |Underscore [105..106)
       |RightArrow [107..109)
       |Ident(baz) [110..113)
       |Indentation.Outdent [113..113)
       |KwFinally [114..121)
       |Ident(qux) [122..125)
       |Indentation.Outdent [125..125)
       |Indentation.Outdent [125..125)
       |KwFinally [126..133)
       |Ident(xyz) [134..137)
       |EOF [138..138)
       |""".stripMargin,
    showFieldName = true,
  )

  checkPositions[Stat](
    """|object Hello {
       |  buffer
       |    += new Object()
       |    += new Object
       |}
       |""".stripMargin,
    """|<templ>Template {
       |  buffer
       |    += new Object()
       |    += new Object
       |}</templ> [13:{...:63)
       |<body>Template.Body {
       |  buffer
       |    += new Object()
       |    += new Object
       |}</body> [13:{...:63)
       |<stats0>Term.ApplyInfix buffer
       |    += new Object()
       |    += new Object</stats0> [17:buffer...:61)
       |<lhs>Term.ApplyInfix buffer
       |    += new Object()</lhs> [17:buffer...:43)
       |<targClause>Type.ArgClause     += @@new Object()</targClause> [31::31)
       |<values0>Term.New new Object()</values0> [31:new Object():43)
       |<init>Init Object()</init> [35:Object():43)
       |<argClauses0>Term.ArgClause ()</argClauses0> [41:():43)
       |<targClause>Type.ArgClause     += @@new Object</targClause> [51::51)
       |<values0>Term.New new Object</values0> [51:new Object:61)
       |""".stripMargin,
    """|BOF [0..0)
       |KwObject [0..6)
       |Ident(Hello) [7..12)
       |LeftBrace [13..14)
       |Ident(buffer) [17..23)
       |InfixLF [23..24)
       |Ident(+=) [28..30)
       |KwNew [31..34)
       |Ident(Object) [35..41)
       |LeftParen [41..42)
       |RightParen [42..43)
       |InfixLF [43..44)
       |Ident(+=) [48..50)
       |KwNew [51..54)
       |Ident(Object) [55..61)
       |LF [61..62)
       |RightBrace [62..63)
       |EOF [64..64)
       |""".stripMargin,
    showPosition = true,
    showFieldName = true,
  )

  // given 3.6+
  checkPositions[Stat](
    """|given Ord[Int]:
       |  def foo = ???
       |""".stripMargin,
    """|Template Ord[Int]:
       |  def foo = ???
       |Init Ord[Int]
       |Type.Apply Ord[Int]
       |Type.ArgClause [Int]
       |Template.Body :
       |  def foo = ???
       |Defn.Def def foo = ???
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|given Ord[Int] {
       |  def foo = ???
       |}
       |""".stripMargin,
    """|Template Ord[Int] {
       |  def foo = ???
       |}
       |Init Ord[Int]
       |Type.Apply Ord[Int]
       |Type.ArgClause [Int]
       |Template.Body {
       |  def foo = ???
       |}
       |Defn.Def def foo = ???
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|given ord: Ord[Int] = ???
       |""".stripMargin,
    """|Type.Apply Ord[Int]
       |Type.ArgClause [Int]
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|given [A : Ord] => Ord[List[A]] = ???
       |""".stripMargin,
    """|Member.ParamClauseGroup [A : Ord] =>
       |Type.ParamClause [A : Ord]
       |Type.Param A : Ord
       |Type.ParamClause given [A @@: Ord] => Ord[List[A]] = ???
       |Type.Bounds : Ord
       |Type.Apply Ord[List[A]]
       |Type.ArgClause [List[A]]
       |Type.Apply List[A]
       |Type.ArgClause [A]
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|given ord: [A : Ord] => Ord[List[A]] = ???
       |""".stripMargin,
    """|Member.ParamClauseGroup [A : Ord] =>
       |Type.ParamClause [A : Ord]
       |Type.Param A : Ord
       |Type.ParamClause given ord: [A @@: Ord] => Ord[List[A]] = ???
       |Type.Bounds : Ord
       |Type.Apply Ord[List[A]]
       |Type.ArgClause [List[A]]
       |Type.Apply List[A]
       |Type.ArgClause [A]
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|given [A] => (ord: Ord[A]) => Ord[List[A]] = ???
       |""".stripMargin,
    """|Member.ParamClauseGroup [A] => (ord: Ord[A]) =>
       |Type.ParamClause [A]
       |Type.ParamClause given [A@@] => (ord: Ord[A]) => Ord[List[A]] = ???
       |Type.Bounds given [A@@] => (ord: Ord[A]) => Ord[List[A]] = ???
       |Term.ParamClause (ord: Ord[A])
       |Type.Apply Ord[A]
       |Type.ArgClause [A]
       |Type.Apply Ord[List[A]]
       |Type.ArgClause [List[A]]
       |Type.Apply List[A]
       |Type.ArgClause [A]
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|given ord: [A] => (ord: Ord[A]) => Ord[List[A]] = ???
       |""".stripMargin,
    """|Member.ParamClauseGroup [A] => (ord: Ord[A]) =>
       |Type.ParamClause [A]
       |Type.ParamClause given ord: [A@@] => (ord: Ord[A]) => Ord[List[A]] = ???
       |Type.Bounds given ord: [A@@] => (ord: Ord[A]) => Ord[List[A]] = ???
       |Term.ParamClause (ord: Ord[A])
       |Type.Apply Ord[A]
       |Type.ArgClause [A]
       |Type.Apply Ord[List[A]]
       |Type.ArgClause [List[A]]
       |Type.Apply List[A]
       |Type.ArgClause [A]
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|given ord: ([A] =>> Ord[A]) => Ord[List[A]] = ???
       |""".stripMargin,
    """|Member.ParamClauseGroup ([A] =>> Ord[A]) =>
       |Type.ParamClause given ord: @@([A] =>> Ord[A]) => Ord[List[A]] = ???
       |Term.ParamClause ([A] =>> Ord[A])
       |Term.Param [A] =>> Ord[A]
       |Type.Lambda [A] =>> Ord[A]
       |Type.ParamClause [A]
       |Type.ParamClause given ord: ([A@@] =>> Ord[A]) => Ord[List[A]] = ???
       |Type.Bounds given ord: ([A@@] =>> Ord[A]) => Ord[List[A]] = ???
       |Type.Apply Ord[A]
       |Type.ArgClause [A]
       |Type.Apply Ord[List[A]]
       |Type.ArgClause [List[A]]
       |Type.Apply List[A]
       |Type.ArgClause [A]
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|given ord: ([A] => (Ord[A]) => Ord[List[A]]) => Ord[List[A]] = ???
       |""".stripMargin,
    """|Member.ParamClauseGroup ([A] => (Ord[A]) => Ord[List[A]]) =>
       |Type.ParamClause given ord: @@([A] => (Ord[A]) => Ord[List[A]]) => Ord[List[A]] = ???
       |Term.ParamClause ([A] => (Ord[A]) => Ord[List[A]])
       |Term.Param [A] => (Ord[A]) => Ord[List[A]]
       |Type.PolyFunction [A] => (Ord[A]) => Ord[List[A]]
       |Type.ParamClause [A]
       |Type.ParamClause given ord: ([A@@] => (Ord[A]) => Ord[List[A]]) => Ord[List[A]] = ???
       |Type.Bounds given ord: ([A@@] => (Ord[A]) => Ord[List[A]]) => Ord[List[A]] = ???
       |Type.Function (Ord[A]) => Ord[List[A]]
       |Type.FuncParamClause (Ord[A])
       |Type.Apply Ord[A]
       |Type.ArgClause [A]
       |Type.Apply Ord[List[A]]
       |Type.ArgClause [List[A]]
       |Type.Apply List[A]
       |Type.ArgClause [A]
       |Type.Apply Ord[List[A]]
       |Type.ArgClause [List[A]]
       |Type.Apply List[A]
       |Type.ArgClause [A]
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|given ord: [A] => (Ord[A] => Ord[List[A]]) = ???
       |""".stripMargin,
    """|Member.ParamClauseGroup [A] =>
       |Type.ParamClause [A]
       |Type.ParamClause given ord: [A@@] => (Ord[A] => Ord[List[A]]) = ???
       |Type.Bounds given ord: [A@@] => (Ord[A] => Ord[List[A]]) = ???
       |Type.Function Ord[A] => Ord[List[A]]
       |Type.FuncParamClause Ord[A]
       |Type.Apply Ord[A]
       |Type.ArgClause [A]
       |Type.Apply Ord[List[A]]
       |Type.ArgClause [List[A]]
       |Type.Apply List[A]
       |Type.ArgClause [A]
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|given ord: (a: A) => Ord[A] => Ord[List[A]] = ???
       |""".stripMargin,
    """|Member.ParamClauseGroup (a: A) => Ord[A] =>
       |Type.ParamClause given ord: @@(a: A) => Ord[A] => Ord[List[A]] = ???
       |Term.ParamClause (a: A)
       |Term.ParamClause Ord[A]
       |Term.Param Ord[A]
       |Type.Apply Ord[A]
       |Type.ArgClause [A]
       |Type.Apply Ord[List[A]]
       |Type.ArgClause [List[A]]
       |Type.Apply List[A]
       |Type.ArgClause [A]
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|given ord: [A] => (A) => Ord[A] => [B <: A] => (a: A, b: B) => Ord[List[B]] = ???
       |""".stripMargin,
    """|Member.ParamClauseGroup [A] => (A) => Ord[A] =>
       |Type.ParamClause [A]
       |Type.ParamClause given ord: [A@@] => (A) => Ord[A] => [B <: A] => (a: A, b: B) => Ord[List[B]] = ???
       |Type.Bounds given ord: [A@@] => (A) => Ord[A] => [B <: A] => (a: A, b: B) => Ord[List[B]] = ???
       |Term.ParamClause (A)
       |Term.Param A
       |Term.ParamClause Ord[A]
       |Term.Param Ord[A]
       |Type.Apply Ord[A]
       |Type.ArgClause [A]
       |Member.ParamClauseGroup [B <: A] => (a: A, b: B) =>
       |Type.ParamClause [B <: A]
       |Type.Param B <: A
       |Type.ParamClause given ord: [A] => (A) => Ord[A] => [B @@<: A] => (a: A, b: B) => Ord[List[B]] = ???
       |Type.Bounds <: A
       |Term.ParamClause (a: A, b: B)
       |Type.Apply Ord[List[B]]
       |Type.ArgClause [List[B]]
       |Type.Apply List[B]
       |Type.ArgClause [B]
       |""".stripMargin,
  )

  locally {
    implicit val dialect: Dialect = dialects.Scala3Future

    checkPositions[Stat](
      "val foo: Int ?->{a} List[Int] = _ ?=> List(1, 2, 3)",
      """|Type.Capturing Int ?->{a} List[Int] [9:Int ?->{a} List[Int]:29)
         |Type.PureContextFunction Int ?->{a} List[Int] [9:Int ?->{a} List[Int]:29)
         |Type.FuncParamClause Int [9:Int:12)
         |Type.Apply List[Int] [20:List[Int]:29)
         |Type.ArgClause [Int] [24:[Int]:29)
         |Type.CapturesSet {a} [16:{a}:19)
         |Term.ContextFunction _ ?=> List(1, 2, 3) [32:_ ?=> List(1, 2, 3):51)
         |Term.ParamClause _ [32:_:33)
         |Term.Param _ [32:_:33)
         |Term.Apply List(1, 2, 3) [38:List(1, 2, 3):51)
         |Term.ArgClause (1, 2, 3) [42:(1, 2, 3):51)
         |""".stripMargin,
      showPosition = true,
    )
  }

  test("#4497") {
    assertTokenizedAsStructureLines(
      """|foo:
         |    bar
         |
         |match
         |    case bar =>
         |""".stripMargin,
      """|BOF [0..0)
         |Ident(foo) [0..3)
         |Colon [3..4)
         |Indentation.Indent [4..4)
         |Ident(bar) [9..12)
         |Indentation.Outdent [13..13)
         |KwMatch [14..19)
         |Indentation.Indent [19..19)
         |KwCase [24..28)
         |Ident(bar) [29..32)
         |RightArrow [33..35)
         |Indentation.Outdent [35..35)
         |EOF [36..36)
         |""".stripMargin,
      classOf[Stat],
    )
  }

  checkPositions[Stat](
    """|{
       |  foo:
       |    bar
       |
       |}
       |
       |match
       |    case bar =>
       |""".stripMargin,
    """|<expr>Term.Block {
       |  foo:
       |    bar
       |
       |}</expr> [0:{...:19)
       |<stats0>Term.Apply foo:
       |    bar</stats0> [4:foo:...:16)
       |<values0>Term.Block :
       |    bar</values0> [7<:...>16)
       |<casesBlock>Term.CasesBlock case bar =></casesBlock> [31:case bar =>:42)
       |<cases0>Case case bar =></cases0> [31:case bar =>:42)
       |<body>Term.Block     case bar =>@@</body> [42::42)
       |""".stripMargin,
    """|BOF [0..0)
       |LeftBrace [0..1)
       |Ident(foo) [4..7)
       |Colon [7..8)
       |Indentation.Indent [8..8)
       |Ident(bar) [13..16)
       |Indentation.Outdent [17..17)
       |RightBrace [18..19)
       |KwMatch [21..26)
       |Indentation.Indent [26..26)
       |KwCase [31..35)
       |Ident(bar) [36..39)
       |RightArrow [40..42)
       |Indentation.Outdent [42..42)
       |EOF [43..43)
       |""".stripMargin,
    showPosition = true,
    showFieldName = true,
  )

  checkPositions[Stat](
    """|def f = new A(1) foo
       |  { x => x }
       |""".stripMargin,
    """|Term.ApplyInfix new A(1) foo
       |  { x => x }
       |Term.New new A(1)
       |Init A(1)
       |Term.ArgClause (1)
       |Type.ArgClause def f = new A(1) foo@@
       |Term.Block { x => x }
       |Term.Function x => x
       |Term.ParamClause x
       |Term.Param x
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = xs.map: x => x
       |  .foo
       |""".stripMargin,
    """|Term.Select xs.map: x => x
       |  .foo [8:xs.map: x => x...:29)
       |Term.Apply xs.map: x => x [8:xs.map: x => x:22)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : x => x [14<: x => x>22)
       |Term.Function x => x [16:x => x:22)
       |Term.ParamClause x [16:x:17)
       |Term.Param x [16:x:17)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map: x => x
       |  + 1
       |""".stripMargin,
    """|Term.ApplyInfix xs.map: x => x
       |  + 1 [8:xs.map: x => x...:28)
       |Term.Apply xs.map: x => x [8:xs.map: x => x:22)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : x => x [14<: x => x>22)
       |Term.Function x => x [16:x => x:22)
       |Term.ParamClause x [16:x:17)
       |Term.Param x [16:x:17)
       |Type.ArgClause   + @@1 [27::27)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    "def f = xs.map: x => x",
    """|Term.Apply xs.map: x => x [8:xs.map: x => x:22)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : x => x [14<: x => x>22)
       |Term.Function x => x [16:x => x:22)
       |Term.ParamClause x [16:x:17)
       |Term.Param x [16:x:17)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map: x => y => x + y
       |""".stripMargin,
    """|Term.Apply xs.map: x => y => x + y [8:xs.map: x => y => x + y:31)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : x => y => x + y [14<: x => y => x + y>31)
       |Term.Function x => y => x + y [16:x => y => x + y:31)
       |Term.ParamClause x [16:x:17)
       |Term.Param x [16:x:17)
       |Term.Function y => x + y [21:y => x + y:31)
       |Term.ParamClause y [21:y:22)
       |Term.Param y [21:y:22)
       |Term.ApplyInfix x + y [26:x + y:31)
       |Type.ArgClause def f = xs.map: x => y => x + @@y [30::30)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map: x => y =>
       |  x + y
       |""".stripMargin,
    """|Term.Apply xs.map: x => y =>
       |  x + y [8:xs.map: x => y =>...:33)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : x => y =>
       |  x + y [14<: x => y =>...>33)
       |Term.Function x => y =>
       |  x + y [16:x => y =>...:33)
       |Term.ParamClause x [16:x:17)
       |Term.Param x [16:x:17)
       |Term.Function y =>
       |  x + y [21:y =>...:33)
       |Term.ParamClause y [21:y:22)
       |Term.Param y [21:y:22)
       |Term.ApplyInfix x + y [28:x + y:33)
       |Type.ArgClause   x + @@y [32::32)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map: [T] => (x: T) =>
       |  x
       |""".stripMargin,
    """|Term.Apply xs.map: [T] => (x: T) =>
       |  x [8:xs.map: [T] => (x: T) =>...:36)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : [T] => (x: T) =>
       |  x [14<: [T] => (x: T) =>...>36)
       |Term.PolyFunction [T] => (x: T) =>
       |  x [16:[T] => (x: T) =>...:36)
       |Type.ParamClause [T] [16:[T]:19)
       |Type.ParamClause def f = xs.map: [T@@] => (x: T) => [18::18)
       |Type.Bounds def f = xs.map: [T@@] => (x: T) => [18::18)
       |Term.Function (x: T) =>
       |  x [23:(x: T) =>...:36)
       |Term.ParamClause (x: T) [23:(x: T):29)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map:
       |  case y => y
       |""".stripMargin,
    """|Term.Apply xs.map:
       |  case y => y [8:xs.map:...:29)
       |Term.Select xs.map [8:xs.map:14)
       |Term.PartialFunction :
       |  case y => y [14<:...>29)
       |Case case y => y [18:case y => y:29)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  case (a, b) => a
       |""".stripMargin,
    """|Term.PartialFunction case (a, b) => a [10:case (a, b) => a:26)
       |Case case (a, b) => a [10:case (a, b) => a:26)
       |Pat.Tuple (a, b) [15:(a, b):21)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map: x => a + ys.map: y => y
       |""".stripMargin,
    """|Term.Apply xs.map: x => a + ys.map: y => y [8:xs.map: x => a + ys.map: y => y:39)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : x => a + ys.map: y => y [14<: x => a + ys.map: y => y>39)
       |Term.Function x => a + ys.map: y => y [16:x => a + ys.map: y => y:39)
       |Term.ParamClause x [16:x:17)
       |Term.Param x [16:x:17)
       |Term.ApplyInfix a + ys.map: y => y [21:a + ys.map: y => y:39)
       |Type.ArgClause def f = xs.map: x => a + @@ys.map: y => y [25::25)
       |Term.Apply ys.map: y => y [25:ys.map: y => y:39)
       |Term.Select ys.map [25:ys.map:31)
       |Term.Block : y => y [31<: y => y>39)
       |Term.Function y => y [33:y => y:39)
       |Term.ParamClause y [33:y:34)
       |Term.Param y [33:y:34)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map: x => foo(x,
       |  1)
       |""".stripMargin,
    """|Term.Apply xs.map: x => foo(x,
       |  1) [8:xs.map: x => foo(x,...:32)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : x => foo(x,
       |  1) [14<: x => foo(x,...>32)
       |Term.Function x => foo(x,
       |  1) [16:x => foo(x,...:32)
       |Term.ParamClause x [16:x:17)
       |Term.Param x [16:x:17)
       |Term.Apply foo(x,
       |  1) [21:foo(x,...:32)
       |Term.ArgClause (x,
       |  1) [24:(x,...:32)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { x: Int =>
       |  x + 1
       |}
       |""".stripMargin,
    """|Term.Apply foo { x: Int =>
       |  x + 1
       |} [8:foo { x: Int =>...:33)
       |Term.Block { x: Int =>
       |  x + 1
       |} [12:{ x: Int =>...:33)
       |Term.Apply x: Int =>
       |  x + 1 [14:x: Int =>...:31)
       |Term.Block : Int =>
       |  x + 1 [15<: Int =>...>31)
       |Term.Function Int =>
       |  x + 1 [17:Int =>...:31)
       |Term.ParamClause Int [17:Int:20)
       |Term.Param Int [17:Int:20)
       |Term.ApplyInfix x + 1 [26:x + 1:31)
       |Type.ArgClause   x + @@1 [30::30)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Source](
    """|object A:
       |  given Foo with { self: Bar => def f = 1 }
       |""".stripMargin,
    """|Defn.Object object A:
       |  given Foo with { self: Bar => def f = 1 } [0:object A:...:53)
       |Template :
       |  given Foo with { self: Bar => def f = 1 } [8<:...>53)
       |Template.Body :
       |  given Foo with { self: Bar => def f = 1 } [8<:...>53)
       |Defn.Given given Foo with { self: Bar => def f = 1 } [12:given Foo with { self: Bar => def f = 1 }:53)
       |Template Foo with { self: Bar => def f = 1 } [18:Foo with { self: Bar => def f = 1 }:53)
       |Template.Body { self: Bar => def f = 1 } [27:{ self: Bar => def f = 1 }:53)
       |Self self: Bar => [29:self: Bar =>:41)
       |Defn.Def def f = 1 [42:def f = 1:51)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { a }: x => x
       |""".stripMargin,
    """|Term.Ascribe foo { a }: x => x [8:foo { a }: x => x:25)
       |Term.Apply foo { a } [8:foo { a }:17)
       |Term.Block { a } [12:{ a }:17)
       |Type.Function x => x [19:x => x:25)
       |Type.FuncParamClause x [19:x:20)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map { implicit x: Int => x }
       |""".stripMargin,
    """|Term.Apply xs.map { implicit x: Int => x } [8:xs.map { implicit x: Int => x }:39)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block { implicit x: Int => x } [15:{ implicit x: Int => x }:39)
       |Term.Function implicit x: Int => x [17:implicit x: Int => x:37)
       |Term.ParamClause implicit x: Int [17:implicit x: Int:32)
       |Term.Param implicit x: Int [17:implicit x: Int:32)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Source](
    """|object A:
       |  given f: Ord[Int] => Ord[List[Int]] = ???
       |""".stripMargin,
    """|Defn.Object object A:
       |  given f: Ord[Int] => Ord[List[Int]] = ??? [0:object A:...:53)
       |Template :
       |  given f: Ord[Int] => Ord[List[Int]] = ??? [8<:...>53)
       |Template.Body :
       |  given f: Ord[Int] => Ord[List[Int]] = ??? [8<:...>53)
       |Defn.GivenAlias given f: Ord[Int] => Ord[List[Int]] = ??? [12:given f: Ord[Int] => Ord[List[Int]] = ???:53)
       |Member.ParamClauseGroup Ord[Int] => [21:Ord[Int] =>:32)
       |Type.ParamClause   given f: @@Ord[Int] => Ord[List[Int]] = ??? [21::21)
       |Term.ParamClause Ord[Int] [21:Ord[Int]:29)
       |Term.Param Ord[Int] [21:Ord[Int]:29)
       |Type.Apply Ord[Int] [21:Ord[Int]:29)
       |Type.ArgClause [Int] [24:[Int]:29)
       |Type.Apply Ord[List[Int]] [33:Ord[List[Int]]:47)
       |Type.ArgClause [List[Int]] [36:[List[Int]]:47)
       |Type.Apply List[Int] [37:List[Int]:46)
       |Type.ArgClause [Int] [41:[Int]:46)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map: x => try a catch case e => b
       |  .foo
       |""".stripMargin,
    """|Term.Select xs.map: x => try a catch case e => b
       |  .foo [8:xs.map: x => try a catch case e => b...:51)
       |Term.Apply xs.map: x => try a catch case e => b [8:xs.map: x => try a catch case e => b:44)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : x => try a catch case e => b [14<: x => try a catch case e => b>44)
       |Term.Function x => try a catch case e => b [16:x => try a catch case e => b:44)
       |Term.ParamClause x [16:x:17)
       |Term.Param x [16:x:17)
       |Term.Try try a catch case e => b [21:try a catch case e => b:44)
       |Term.CasesBlock case e => b [33:case e => b:44)
       |Case case e => b [33:case e => b:44)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map: x => x
       |  // comment
       |  .foo
       |""".stripMargin,
    """|Term.Select xs.map: x => x
       |  // comment
       |  .foo [8:xs.map: x => x...:42)
       |Term.Apply xs.map: x => x [8:xs.map: x => x:22)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : x => x [14<: x => x>22)
       |Term.Function x => x [16:x => x:22)
       |Term.ParamClause x [16:x:17)
       |Term.Param x [16:x:17)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = for (x <- xs.map: y => y) yield x
       |""".stripMargin,
    """|Term.ForYield for (x <- xs.map: y => y) yield x [8:for (x <- xs.map: y => y) yield x:41)
       |Term.EnumeratorsBlock (x <- xs.map: y => y) [12:(x <- xs.map: y => y):33)
       |Enumerator.Generator x <- xs.map: y => y [13:x <- xs.map: y => y:32)
       |Term.Ascribe xs.map: y => y [18:xs.map: y => y:32)
       |Term.Select xs.map [18:xs.map:24)
       |Type.Function y => y [26:y => y:32)
       |Type.FuncParamClause y [26:y:27)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map: (x: Int,
       |  y: Int) => x
       |""".stripMargin,
    """|Term.Apply xs.map: (x: Int,
       |  y: Int) => x [8:xs.map: (x: Int,...:39)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : (x: Int,
       |  y: Int) => x [14<: (x: Int,...>39)
       |Term.Function (x: Int,
       |  y: Int) => x [16:(x: Int,...:39)
       |Term.ParamClause (x: Int,
       |  y: Int) [16:(x: Int,...:34)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val x: Int = foo: y => y
       |""".stripMargin,
    """|Term.Apply foo: y => y [13:foo: y => y:24)
       |Term.Block : y => y [16<: y => y>24)
       |Term.Function y => y [18:y => y:24)
       |Term.ParamClause y [18:y:19)
       |Term.Param y [18:y:19)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map: x => x match { case 1 => 2 }
       |  .foo
       |""".stripMargin,
    """|Term.Select xs.map: x => x match { case 1 => 2 }
       |  .foo [8:xs.map: x => x match { case 1 => 2 }...:51)
       |Term.Apply xs.map: x => x match { case 1 => 2 } [8:xs.map: x => x match { case 1 => 2 }:44)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : x => x match { case 1 => 2 } [14<: x => x match { case 1 => 2 }>44)
       |Term.Function x => x match { case 1 => 2 } [16:x => x match { case 1 => 2 }:44)
       |Term.ParamClause x [16:x:17)
       |Term.Param x [16:x:17)
       |Term.Match x match { case 1 => 2 } [21:x match { case 1 => 2 }:44)
       |Term.CasesBlock { case 1 => 2 } [29:{ case 1 => 2 }:44)
       |Case case 1 => 2 [31:case 1 => 2:42)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Source](
    """|object A:
       |  this: B => def f = 1
       |""".stripMargin,
    """|Defn.Object object A:
       |  this: B => def f = 1 [0:object A:...:32)
       |Template :
       |  this: B => def f = 1 [8<:...>32)
       |Template.Body :
       |  this: B => def f = 1 [8<:...>32)
       |Self this: B => [12:this: B =>:22)
       |Defn.Def def f = 1 [23:def f = 1:32)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map: x => /* a
       |  b */ x
       |""".stripMargin,
    """|Term.Apply xs.map: x => /* a
       |  b */ x [8:xs.map: x => /* a...:34)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : x => /* a
       |  b */ x [14<: x => /* a...>34)
       |Term.Function x => /* a
       |  b */ x [16:x => /* a...:34)
       |Term.ParamClause x [16:x:17)
       |Term.Param x [16:x:17)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val f = (xs.map: x => x)
       |""".stripMargin,
    """|Term.Ascribe xs.map: x => x [9:xs.map: x => x:23)
       |Term.Select xs.map [9:xs.map:15)
       |Type.Function x => x [17:x => x:23)
       |Type.FuncParamClause x [17:x:18)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Source](
    """|object A:
       |  _: B => def f = 1
       |""".stripMargin,
    """|Defn.Object object A:
       |  _: B => def f = 1 [0:object A:...:29)
       |Template :
       |  _: B => def f = 1 [8<:...>29)
       |Template.Body :
       |  _: B => def f = 1 [8<:...>29)
       |Self _: B => [12:_: B =>:19)
       |Defn.Def def f = 1 [20:def f = 1:29)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map: x => // c
       |  x + 1
       |""".stripMargin,
    """|Term.Apply xs.map: x => // c
       |  x + 1 [8:xs.map: x => // c...:33)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : x => // c
       |  x + 1 [14<: x => // c...>33)
       |Term.Function x => // c
       |  x + 1 [16:x => // c...:33)
       |Term.ParamClause x [16:x:17)
       |Term.Param x [16:x:17)
       |Term.ApplyInfix x + 1 [28:x + 1:33)
       |Type.ArgClause   x + @@1 [32::32)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = try a catch case e => b
       |  .foo
       |""".stripMargin,
    """|Term.Try try a catch case e => b
       |  .foo [8:try a catch case e => b...:38)
       |Term.CasesBlock case e => b
       |  .foo [20:case e => b...:38)
       |Case case e => b
       |  .foo [20:case e => b...:38)
       |Term.Select b
       |  .foo [30:b...:38)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = !1:
       |  x => x
       |""".stripMargin,
    """|Term.Ascribe !1:
       |  x => x [8:!1:...:20)
       |Lit.WithUnary !1 [8:!1:10)
       |Type.Function x => x [14:x => x:20)
       |Type.FuncParamClause x [14:x:15)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val x = y: Int ?=> Boolean
       |""".stripMargin,
    """|Term.Apply y: Int ?=> Boolean [8:y: Int ?=> Boolean:26)
       |Term.Block : Int ?=> Boolean [9<: Int ?=> Boolean>26)
       |Term.ContextFunction Int ?=> Boolean [11:Int ?=> Boolean:26)
       |Term.ParamClause Int [11:Int:14)
       |Term.Param Int [11:Int:14)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case 1 => a
       |  case y: Int => b
       |""".stripMargin,
    """|Term.Match x match
       |  case 1 => a
       |  case y: Int => b [8:x match...:48)
       |Term.CasesBlock case 1 => a
       |  case y: Int => b [18:case 1 => a...:48)
       |Case case 1 => a [18:case 1 => a:29)
       |Case case y: Int => b [32:case y: Int => b:48)
       |Pat.Typed y: Int [37:y: Int:43)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map: x =>  y => x + y
       |""".stripMargin,
    """|Term.Apply xs.map: x =>  y => x + y [8:xs.map: x =>  y => x + y:32)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block : x =>  y => x + y [14<: x =>  y => x + y>32)
       |Term.Function x =>  y => x + y [16:x =>  y => x + y:32)
       |Term.ParamClause x [16:x:17)
       |Term.Param x [16:x:17)
       |Term.Function y => x + y [22:y => x + y:32)
       |Term.ParamClause y [22:y:23)
       |Term.Param y [22:y:23)
       |Term.ApplyInfix x + y [27:x + y:32)
       |Type.ArgClause def f = xs.map: x =>  y => x + @@y [31::31)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Source](
    """|object A:
       |  def f = try a catch case e => b; c
       |""".stripMargin,
    """|Defn.Object object A:
       |  def f = try a catch case e => b; c [0:object A:...:46)
       |Template :
       |  def f = try a catch case e => b; c [8<:...>46)
       |Template.Body :
       |  def f = try a catch case e => b; c [8<:...>46)
       |Defn.Def def f = try a catch case e => b [12:def f = try a catch case e => b:43)
       |Term.Try try a catch case e => b [20:try a catch case e => b:43)
       |Term.CasesBlock case e => b [32:case e => b:43)
       |Case case e => b [32:case e => b:43)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  try a catch case e => b
       |    c
       |  d
       |""".stripMargin,
    """|Term.Block try a catch case e => b
       |    c
       |  d [10:try a catch case e => b...:43)
       |Term.Try try a catch case e => b [10:try a catch case e => b:33)
       |Term.CasesBlock case e => b [22:case e => b:33)
       |Case case e => b [22:case e => b:33)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  g(xs.map: x =>
       |    x)
       |""".stripMargin,
    """|Term.Apply g(xs.map: x =>
       |    x) [10:g(xs.map: x =>...:31)
       |Term.ArgClause (xs.map: x =>
       |    x) [11:(xs.map: x =>...:31)
       |Term.Apply xs.map: x =>
       |    x [12:xs.map: x =>...:30)
       |Term.Select xs.map [12:xs.map:18)
       |Term.Block : x =>
       |    x [18<: x =>...>30)
       |Term.Function x =>
       |    x [20:x =>...:30)
       |Term.ParamClause x [20:x:21)
       |Term.Param x [20:x:21)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |Ident(g) [10..11)
       |LeftParen [11..12)
       |Ident(xs) [12..14)
       |Dot [14..15)
       |Ident(map) [15..18)
       |Colon [18..19)
       |Ident(x) [20..21)
       |RightArrow [22..24)
       |Indentation.Indent [24..24)
       |Ident(x) [29..30)
       |Indentation.Outdent [30..30)
       |RightParen [30..31)
       |Indentation.Outdent [31..31)
       |EOF [32..32)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  g(xs.map: x =>
       |    x )
       |""".stripMargin,
    """|Term.Apply g(xs.map: x =>
       |    x ) [10:g(xs.map: x =>...:32)
       |Term.ArgClause (xs.map: x =>
       |    x ) [11:(xs.map: x =>...:32)
       |Term.Apply xs.map: x =>
       |    x [12:xs.map: x =>...:30)
       |Term.Select xs.map [12:xs.map:18)
       |Term.Block : x =>
       |    x [18<: x =>...>30)
       |Term.Function x =>
       |    x [20:x =>...:30)
       |Term.ParamClause x [20:x:21)
       |Term.Param x [20:x:21)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |Ident(g) [10..11)
       |LeftParen [11..12)
       |Ident(xs) [12..14)
       |Dot [14..15)
       |Ident(map) [15..18)
       |Colon [18..19)
       |Ident(x) [20..21)
       |RightArrow [22..24)
       |Indentation.Indent [24..24)
       |Ident(x) [29..30)
       |Indentation.Outdent [30..30)
       |RightParen [31..32)
       |Indentation.Outdent [32..32)
       |EOF [33..33)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  g(xs.map: x =>
       |    if (a) b )
       |""".stripMargin,
    """|Term.Apply g(xs.map: x =>
       |    if (a) b ) [10:g(xs.map: x =>...:39)
       |Term.ArgClause (xs.map: x =>
       |    if (a) b ) [11:(xs.map: x =>...:39)
       |Term.Apply xs.map: x =>
       |    if (a) b [12:xs.map: x =>...:37)
       |Term.Select xs.map [12:xs.map:18)
       |Term.Block : x =>
       |    if (a) b [18<: x =>...>37)
       |Term.Function x =>
       |    if (a) b [20:x =>...:37)
       |Term.ParamClause x [20:x:21)
       |Term.Param x [20:x:21)
       |Term.If if (a) b [29:if (a) b:37)
       |Lit.Unit     if (a) b@@ ) [37::37)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |Ident(g) [10..11)
       |LeftParen [11..12)
       |Ident(xs) [12..14)
       |Dot [14..15)
       |Ident(map) [15..18)
       |Colon [18..19)
       |Ident(x) [20..21)
       |RightArrow [22..24)
       |Indentation.Indent [24..24)
       |KwIf [29..31)
       |LeftParen [32..33)
       |Ident(a) [33..34)
       |RightParen [34..35)
       |Ident(b) [36..37)
       |Indentation.Outdent [37..37)
       |RightParen [38..39)
       |Indentation.Outdent [39..39)
       |EOF [40..40)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  g(xs.map: x =>
       |    if (a) b /* c */ )
       |""".stripMargin,
    """|Term.Apply g(xs.map: x =>
       |    if (a) b /* c */ ) [10:g(xs.map: x =>...:47)
       |Term.ArgClause (xs.map: x =>
       |    if (a) b /* c */ ) [11:(xs.map: x =>...:47)
       |Term.Apply xs.map: x =>
       |    if (a) b [12:xs.map: x =>...:37)
       |Term.Select xs.map [12:xs.map:18)
       |Term.Block : x =>
       |    if (a) b [18<: x =>...>37)
       |Term.Function x =>
       |    if (a) b [20:x =>...:37)
       |Term.ParamClause x [20:x:21)
       |Term.Param x [20:x:21)
       |Term.If if (a) b [29:if (a) b:37)
       |Lit.Unit     if (a) b /* c */@@ ) [45::45)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |Ident(g) [10..11)
       |LeftParen [11..12)
       |Ident(xs) [12..14)
       |Dot [14..15)
       |Ident(map) [15..18)
       |Colon [18..19)
       |Ident(x) [20..21)
       |RightArrow [22..24)
       |Indentation.Indent [24..24)
       |KwIf [29..31)
       |LeftParen [32..33)
       |Ident(a) [33..34)
       |RightParen [34..35)
       |Ident(b) [36..37)
       |Indentation.Outdent [45..45)
       |RightParen [46..47)
       |Indentation.Outdent [47..47)
       |EOF [48..48)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  { xs.map: x =>
       |    x }
       |""".stripMargin,
    """|Term.Block { xs.map: x =>
       |    x } [10:{ xs.map: x =>...:32)
       |Term.Apply xs.map: x =>
       |    x [12:xs.map: x =>...:30)
       |Term.Select xs.map [12:xs.map:18)
       |Term.Block : x =>
       |    x [18<: x =>...>30)
       |Term.Function x =>
       |    x [20:x =>...:30)
       |Term.ParamClause x [20:x:21)
       |Term.Param x [20:x:21)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |LeftBrace [10..11)
       |Ident(xs) [12..14)
       |Dot [14..15)
       |Ident(map) [15..18)
       |Colon [18..19)
       |Ident(x) [20..21)
       |RightArrow [22..24)
       |Indentation.Indent [24..24)
       |Ident(x) [29..30)
       |Indentation.Outdent [30..30)
       |RightBrace [31..32)
       |Indentation.Outdent [32..32)
       |EOF [33..33)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  { xs.map: x =>
       |    if (a) b }
       |""".stripMargin,
    """|Term.Block { xs.map: x =>
       |    if (a) b } [10:{ xs.map: x =>...:39)
       |Term.Apply xs.map: x =>
       |    if (a) b [12:xs.map: x =>...:37)
       |Term.Select xs.map [12:xs.map:18)
       |Term.Block : x =>
       |    if (a) b [18<: x =>...>37)
       |Term.Function x =>
       |    if (a) b [20:x =>...:37)
       |Term.ParamClause x [20:x:21)
       |Term.Param x [20:x:21)
       |Term.If if (a) b [29:if (a) b:37)
       |Lit.Unit     if (a) b@@ } [37::37)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |LeftBrace [10..11)
       |Ident(xs) [12..14)
       |Dot [14..15)
       |Ident(map) [15..18)
       |Colon [18..19)
       |Ident(x) [20..21)
       |RightArrow [22..24)
       |Indentation.Indent [24..24)
       |KwIf [29..31)
       |LeftParen [32..33)
       |Ident(a) [33..34)
       |RightParen [34..35)
       |Ident(b) [36..37)
       |Indentation.Outdent [37..37)
       |RightBrace [38..39)
       |Indentation.Outdent [39..39)
       |EOF [40..40)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  g(xs.map: x =>
       |    x , 2)
       |""".stripMargin,
    """|Term.Apply g(xs.map: x =>
       |    x , 2) [10:g(xs.map: x =>...:35)
       |Term.ArgClause (xs.map: x =>
       |    x , 2) [11:(xs.map: x =>...:35)
       |Term.Apply xs.map: x =>
       |    x [12:xs.map: x =>...:30)
       |Term.Select xs.map [12:xs.map:18)
       |Term.Block : x =>
       |    x [18<: x =>...>30)
       |Term.Function x =>
       |    x [20:x =>...:30)
       |Term.ParamClause x [20:x:21)
       |Term.Param x [20:x:21)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |Ident(g) [10..11)
       |LeftParen [11..12)
       |Ident(xs) [12..14)
       |Dot [14..15)
       |Ident(map) [15..18)
       |Colon [18..19)
       |Ident(x) [20..21)
       |RightArrow [22..24)
       |Indentation.Indent [24..24)
       |Ident(x) [29..30)
       |Indentation.Outdent [30..30)
       |Comma [31..32)
       |Constant.Int(2) [33..34)
       |RightParen [34..35)
       |Indentation.Outdent [35..35)
       |EOF [36..36)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  if xs.exists: x =>
       |    x then 1 else 2
       |""".stripMargin,
    """|Term.If if xs.exists: x =>
       |    x then 1 else 2 [10:if xs.exists: x =>...:48)
       |Term.Apply xs.exists: x =>
       |    x [13:xs.exists: x =>...:34)
       |Term.Select xs.exists [13:xs.exists:22)
       |Term.Block : x =>
       |    x [22<: x =>...>34)
       |Term.Function x =>
       |    x [24:x =>...:34)
       |Term.ParamClause x [24:x:25)
       |Term.Param x [24:x:25)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |KwIf [10..12)
       |Ident(xs) [13..15)
       |Dot [15..16)
       |Ident(exists) [16..22)
       |Colon [22..23)
       |Ident(x) [24..25)
       |RightArrow [26..28)
       |Indentation.Indent [28..28)
       |Ident(x) [33..34)
       |Indentation.Outdent [34..34)
       |KwThen [35..39)
       |Constant.Int(1) [40..41)
       |KwElse [42..46)
       |Constant.Int(2) [47..48)
       |Indentation.Outdent [48..48)
       |EOF [49..49)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  while xs.exists: x =>
       |    if (a) b do ()
       |""".stripMargin,
    """|Term.While while xs.exists: x =>
       |    if (a) b do () [10:while xs.exists: x =>...:50)
       |Term.Apply xs.exists: x =>
       |    if (a) b [16:xs.exists: x =>...:44)
       |Term.Select xs.exists [16:xs.exists:25)
       |Term.Block : x =>
       |    if (a) b [25<: x =>...>44)
       |Term.Function x =>
       |    if (a) b [27:x =>...:44)
       |Term.ParamClause x [27:x:28)
       |Term.Param x [27:x:28)
       |Term.If if (a) b [36:if (a) b:44)
       |Lit.Unit     if (a) b@@ do () [44::44)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |KwWhile [10..15)
       |Ident(xs) [16..18)
       |Dot [18..19)
       |Ident(exists) [19..25)
       |Colon [25..26)
       |Ident(x) [27..28)
       |RightArrow [29..31)
       |Indentation.Indent [31..31)
       |KwIf [36..38)
       |LeftParen [39..40)
       |Ident(a) [40..41)
       |RightParen [41..42)
       |Ident(b) [43..44)
       |Indentation.Outdent [44..44)
       |KwDo [45..47)
       |LeftParen [48..49)
       |RightParen [49..50)
       |Indentation.Outdent [50..50)
       |EOF [51..51)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  for y <- xs.map: x =>
       |    if (a) b yield y
       |""".stripMargin,
    """|Term.ForYield for y <- xs.map: x =>
       |    if (a) b yield y [10:for y <- xs.map: x =>...:52)
       |Term.EnumeratorsBlock y <- xs.map: x =>
       |    if (a) b [14:y <- xs.map: x =>...:44)
       |Enumerator.Generator y <- xs.map: x =>
       |    if (a) b [14:y <- xs.map: x =>...:44)
       |Term.Apply xs.map: x =>
       |    if (a) b [19:xs.map: x =>...:44)
       |Term.Select xs.map [19:xs.map:25)
       |Term.Block : x =>
       |    if (a) b [25<: x =>...>44)
       |Term.Function x =>
       |    if (a) b [27:x =>...:44)
       |Term.ParamClause x [27:x:28)
       |Term.Param x [27:x:28)
       |Term.If if (a) b [36:if (a) b:44)
       |Lit.Unit     if (a) b@@ yield y [44::44)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |KwFor [10..13)
       |Ident(y) [14..15)
       |LeftArrow [16..18)
       |Ident(xs) [19..21)
       |Dot [21..22)
       |Ident(map) [22..25)
       |Colon [25..26)
       |Ident(x) [27..28)
       |RightArrow [29..31)
       |Indentation.Indent [31..31)
       |KwIf [36..38)
       |LeftParen [39..40)
       |Ident(a) [40..41)
       |RightParen [41..42)
       |Ident(b) [43..44)
       |Indentation.Outdent [44..44)
       |KwYield [45..50)
       |Ident(y) [51..52)
       |Indentation.Outdent [52..52)
       |EOF [53..53)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  try a catch case _ => x finally c
       |""".stripMargin,
    """|Term.Try try a catch case _ => x finally c [10:try a catch case _ => x finally c:43)
       |Term.CasesBlock case _ => x [22:case _ => x:33)
       |Case case _ => x [22:case _ => x:33)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |KwTry [10..13)
       |Ident(a) [14..15)
       |KwCatch [16..21)
       |Indentation.Indent [22..22)
       |KwCase [22..26)
       |Underscore [27..28)
       |RightArrow [29..31)
       |Ident(x) [32..33)
       |Indentation.Outdent [33..33)
       |KwFinally [34..41)
       |Ident(c) [42..43)
       |Indentation.Outdent [43..43)
       |EOF [44..44)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  try a catch case _ => h(x) finally c
       |""".stripMargin,
    """|Term.Try try a catch case _ => h(x) finally c [10:try a catch case _ => h(x) finally c:46)
       |Term.CasesBlock case _ => h(x) [22:case _ => h(x):36)
       |Case case _ => h(x) [22:case _ => h(x):36)
       |Term.Apply h(x) [32:h(x):36)
       |Term.ArgClause (x) [33:(x):36)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |KwTry [10..13)
       |Ident(a) [14..15)
       |KwCatch [16..21)
       |Indentation.Indent [22..22)
       |KwCase [22..26)
       |Underscore [27..28)
       |RightArrow [29..31)
       |Ident(h) [32..33)
       |LeftParen [33..34)
       |Ident(x) [34..35)
       |RightParen [35..36)
       |Indentation.Outdent [36..36)
       |KwFinally [37..44)
       |Ident(c) [45..46)
       |Indentation.Outdent [46..46)
       |EOF [47..47)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  g(x match
       |    case 1 =>)
       |""".stripMargin,
    """|Term.Apply g(x match
       |    case 1 =>) [10:g(x match...:34)
       |Term.ArgClause (x match
       |    case 1 =>) [11:(x match...:34)
       |Term.Match x match
       |    case 1 => [12:x match...:33)
       |Term.CasesBlock case 1 => [24:case 1 =>:33)
       |Case case 1 => [24:case 1 =>:33)
       |Term.Block     case 1 =>@@) [33::33)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |Ident(g) [10..11)
       |LeftParen [11..12)
       |Ident(x) [12..13)
       |KwMatch [14..19)
       |Indentation.Indent [19..19)
       |KwCase [24..28)
       |Constant.Int(1) [29..30)
       |RightArrow [31..33)
       |Indentation.Outdent [33..33)
       |RightParen [33..34)
       |Indentation.Outdent [34..34)
       |EOF [35..35)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  g(x match
       |    case 1 => )
       |""".stripMargin,
    """|Term.Apply g(x match
       |    case 1 => ) [10:g(x match...:35)
       |Term.ArgClause (x match
       |    case 1 => ) [11:(x match...:35)
       |Term.Match x match
       |    case 1 => [12:x match...:33)
       |Term.CasesBlock case 1 => [24:case 1 =>:33)
       |Case case 1 => [24:case 1 =>:33)
       |Term.Block     case 1 =>@@ ) [33::33)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |Ident(g) [10..11)
       |LeftParen [11..12)
       |Ident(x) [12..13)
       |KwMatch [14..19)
       |Indentation.Indent [19..19)
       |KwCase [24..28)
       |Constant.Int(1) [29..30)
       |RightArrow [31..33)
       |Indentation.Outdent [33..33)
       |RightParen [34..35)
       |Indentation.Outdent [35..35)
       |EOF [36..36)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  g(xs.map: x =>
       |    return)
       |""".stripMargin,
    """|Term.Apply g(xs.map: x =>
       |    return) [10:g(xs.map: x =>...:36)
       |Term.ArgClause (xs.map: x =>
       |    return) [11:(xs.map: x =>...:36)
       |Term.Apply xs.map: x =>
       |    return [12:xs.map: x =>...:35)
       |Term.Select xs.map [12:xs.map:18)
       |Term.Block : x =>
       |    return [18<: x =>...>35)
       |Term.Function x =>
       |    return [20:x =>...:35)
       |Term.ParamClause x [20:x:21)
       |Term.Param x [20:x:21)
       |Term.Return return [29:return:35)
       |Lit.Unit     return@@) [35::35)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |Ident(g) [10..11)
       |LeftParen [11..12)
       |Ident(xs) [12..14)
       |Dot [14..15)
       |Ident(map) [15..18)
       |Colon [18..19)
       |Ident(x) [20..21)
       |RightArrow [22..24)
       |Indentation.Indent [24..24)
       |KwReturn [29..35)
       |Indentation.Outdent [35..35)
       |RightParen [35..36)
       |Indentation.Outdent [36..36)
       |EOF [37..37)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  g(xs.map: x =>
       |    if (a) b /* c */)
       |""".stripMargin,
    """|Term.Apply g(xs.map: x =>
       |    if (a) b /* c */) [10:g(xs.map: x =>...:46)
       |Term.ArgClause (xs.map: x =>
       |    if (a) b /* c */) [11:(xs.map: x =>...:46)
       |Term.Apply xs.map: x =>
       |    if (a) b [12:xs.map: x =>...:37)
       |Term.Select xs.map [12:xs.map:18)
       |Term.Block : x =>
       |    if (a) b [18<: x =>...>37)
       |Term.Function x =>
       |    if (a) b [20:x =>...:37)
       |Term.ParamClause x [20:x:21)
       |Term.Param x [20:x:21)
       |Term.If if (a) b [29:if (a) b:37)
       |Lit.Unit     if (a) b /* c */@@) [45::45)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |Ident(g) [10..11)
       |LeftParen [11..12)
       |Ident(xs) [12..14)
       |Dot [14..15)
       |Ident(map) [15..18)
       |Colon [18..19)
       |Ident(x) [20..21)
       |RightArrow [22..24)
       |Indentation.Indent [24..24)
       |KwIf [29..31)
       |LeftParen [32..33)
       |Ident(a) [33..34)
       |RightParen [34..35)
       |Ident(b) [36..37)
       |Indentation.Outdent [45..45)
       |RightParen [45..46)
       |Indentation.Outdent [46..46)
       |EOF [47..47)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  { xs.map: x =>
       |    x /* c */}
       |""".stripMargin,
    """|Term.Block { xs.map: x =>
       |    x /* c */} [10:{ xs.map: x =>...:39)
       |Term.Apply xs.map: x =>
       |    x [12:xs.map: x =>...:30)
       |Term.Select xs.map [12:xs.map:18)
       |Term.Block : x =>
       |    x [18<: x =>...>30)
       |Term.Function x =>
       |    x [20:x =>...:30)
       |Term.ParamClause x [20:x:21)
       |Term.Param x [20:x:21)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |LeftBrace [10..11)
       |Ident(xs) [12..14)
       |Dot [14..15)
       |Ident(map) [15..18)
       |Colon [18..19)
       |Ident(x) [20..21)
       |RightArrow [22..24)
       |Indentation.Indent [24..24)
       |Ident(x) [29..30)
       |Indentation.Outdent [38..38)
       |RightBrace [38..39)
       |Indentation.Outdent [39..39)
       |EOF [40..40)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = g { (x: Int) ?=>
       |    a
       |    // c
       |}
       |""".stripMargin,
    """|Term.Apply g { (x: Int) ?=>
       |    a
       |    // c
       |} [8:g { (x: Int) ?=>...:41)
       |Term.Block { (x: Int) ?=>
       |    a
       |    // c
       |} [10:{ (x: Int) ?=>...:41)
       |Term.ContextFunction (x: Int) ?=>
       |    a
       |    // c [12:(x: Int) ?=>...:39)
       |Term.ParamClause (x: Int) [12:(x: Int):20)
       |Term.Block a
       |    // c [29:a...:39)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(g) [8..9)
       |LeftBrace [10..11)
       |LeftParen [12..13)
       |Ident(x) [13..14)
       |Colon [14..15)
       |Ident(Int) [16..19)
       |RightParen [19..20)
       |ContextArrow [21..24)
       |Indentation.Indent [24..24)
       |Ident(a) [29..30)
       |Indentation.Outdent [39..39)
       |RightBrace [40..41)
       |EOF [42..42)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map { x =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply xs.map { x =>
       |  a
       |  // c
       |} [8:xs.map { x =>...:34)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block { x =>
       |  a
       |  // c
       |} [15:{ x =>...:34)
       |Term.Function x =>
       |  a
       |  // c [17:x =>...:32)
       |Term.ParamClause x [17:x:18)
       |Term.Param x [17:x:18)
       |Term.Block a
       |  // c [24:a...:32)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(xs) [8..10)
       |Dot [10..11)
       |Ident(map) [11..14)
       |LeftBrace [15..16)
       |Ident(x) [17..18)
       |RightArrow [19..21)
       |Indentation.Indent [21..21)
       |Ident(a) [24..25)
       |Indentation.Outdent [32..32)
       |RightBrace [33..34)
       |EOF [35..35)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map { x =>
       |  a
       |  b
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply xs.map { x =>
       |  a
       |  b
       |  // c
       |} [8:xs.map { x =>...:38)
       |Term.Select xs.map [8:xs.map:14)
       |Term.Block { x =>
       |  a
       |  b
       |  // c
       |} [15:{ x =>...:38)
       |Term.Function x =>
       |  a
       |  b
       |  // c [17:x =>...:36)
       |Term.ParamClause x [17:x:18)
       |Term.Param x [17:x:18)
       |Term.Block a
       |  b
       |  // c [24:a...:36)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(xs) [8..10)
       |Dot [10..11)
       |Ident(map) [11..14)
       |LeftBrace [15..16)
       |Ident(x) [17..18)
       |RightArrow [19..21)
       |Indentation.Indent [21..21)
       |Ident(a) [24..25)
       |LF [25..26)
       |Ident(b) [28..29)
       |Indentation.Outdent [36..36)
       |RightBrace [37..38)
       |EOF [39..39)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo(xs.map { x =>
       |  a
       |  // c
       |})
       |""".stripMargin,
    """|Term.Apply foo(xs.map { x =>
       |  a
       |  // c
       |}) [8:foo(xs.map { x =>...:39)
       |Term.ArgClause (xs.map { x =>
       |  a
       |  // c
       |}) [11:(xs.map { x =>...:39)
       |Term.Apply xs.map { x =>
       |  a
       |  // c
       |} [12:xs.map { x =>...:38)
       |Term.Select xs.map [12:xs.map:18)
       |Term.Block { x =>
       |  a
       |  // c
       |} [19:{ x =>...:38)
       |Term.Function x =>
       |  a
       |  // c [21:x =>...:36)
       |Term.ParamClause x [21:x:22)
       |Term.Param x [21:x:22)
       |Term.Block a
       |  // c [28:a...:36)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftParen [11..12)
       |Ident(xs) [12..14)
       |Dot [14..15)
       |Ident(map) [15..18)
       |LeftBrace [19..20)
       |Ident(x) [21..22)
       |RightArrow [23..25)
       |Indentation.Indent [25..25)
       |Ident(a) [28..29)
       |Indentation.Outdent [36..36)
       |RightBrace [37..38)
       |RightParen [38..39)
       |EOF [40..40)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.foreach { x =>
       |  a
       |
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply xs.foreach { x =>
       |  a
       |
       |  // c
       |} [8:xs.foreach { x =>...:39)
       |Term.Select xs.foreach [8:xs.foreach:18)
       |Term.Block { x =>
       |  a
       |
       |  // c
       |} [19:{ x =>...:39)
       |Term.Function x =>
       |  a
       |
       |  // c [21:x =>...:37)
       |Term.ParamClause x [21:x:22)
       |Term.Param x [21:x:22)
       |Term.Block a
       |
       |  // c [28:a...:37)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(xs) [8..10)
       |Dot [10..11)
       |Ident(foreach) [11..18)
       |LeftBrace [19..20)
       |Ident(x) [21..22)
       |RightArrow [23..25)
       |Indentation.Indent [25..25)
       |Ident(a) [28..29)
       |Indentation.Outdent [37..37)
       |RightBrace [38..39)
       |EOF [40..40)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = g { (x: Int) ?=>
       |  a
       |
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply g { (x: Int) ?=>
       |  a
       |
       |  // c
       |} [8:g { (x: Int) ?=>...:38)
       |Term.Block { (x: Int) ?=>
       |  a
       |
       |  // c
       |} [10:{ (x: Int) ?=>...:38)
       |Term.ContextFunction (x: Int) ?=>
       |  a
       |
       |  // c [12:(x: Int) ?=>...:36)
       |Term.ParamClause (x: Int) [12:(x: Int):20)
       |Term.Block a
       |
       |  // c [27:a...:36)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(g) [8..9)
       |LeftBrace [10..11)
       |LeftParen [12..13)
       |Ident(x) [13..14)
       |Colon [14..15)
       |Ident(Int) [16..19)
       |RightParen [19..20)
       |ContextArrow [21..24)
       |Indentation.Indent [24..24)
       |Ident(a) [27..28)
       |Indentation.Outdent [36..36)
       |RightBrace [37..38)
       |EOF [39..39)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map { case x =>
       |  a
       |
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply xs.map { case x =>
       |  a
       |
       |  // c
       |} [8:xs.map { case x =>...:40)
       |Term.Select xs.map [8:xs.map:14)
       |Term.PartialFunction { case x =>
       |  a
       |
       |  // c
       |} [15:{ case x =>...:40)
       |Case case x =>
       |  a [17:case x =>...:30)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(xs) [8..10)
       |Dot [10..11)
       |Ident(map) [11..14)
       |LeftBrace [15..16)
       |KwCase [17..21)
       |Ident(x) [22..23)
       |RightArrow [24..26)
       |Ident(a) [29..30)
       |LFLF [30..39)
       |RightBrace [39..40)
       |EOF [41..41)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.foreach { x => x match
       |  case 1 => a
       |  case _ => b
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply xs.foreach { x => x match
       |  case 1 => a
       |  case _ => b
       |  // c
       |} [8:xs.foreach { x => x match...:70)
       |Term.Select xs.foreach [8:xs.foreach:18)
       |Term.Block { x => x match
       |  case 1 => a
       |  case _ => b
       |  // c
       |} [19:{ x => x match...:70)
       |Term.Function x => x match
       |  case 1 => a
       |  case _ => b
       |  // c [21:x => x match...:68)
       |Term.ParamClause x [21:x:22)
       |Term.Param x [21:x:22)
       |Term.Match x match
       |  case 1 => a
       |  case _ => b
       |  // c [26:x match...:68)
       |Term.CasesBlock case 1 => a
       |  case _ => b
       |  // c [36:case 1 => a...:68)
       |Case case 1 => a [36:case 1 => a:47)
       |Case case _ => b [50:case _ => b:61)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(xs) [8..10)
       |Dot [10..11)
       |Ident(foreach) [11..18)
       |LeftBrace [19..20)
       |Ident(x) [21..22)
       |RightArrow [23..25)
       |Ident(x) [26..27)
       |KwMatch [28..33)
       |Indentation.Indent [33..33)
       |KwCase [36..40)
       |Constant.Int(1) [41..42)
       |RightArrow [43..45)
       |Ident(a) [46..47)
       |LF [47..48)
       |KwCase [50..54)
       |Underscore [55..56)
       |RightArrow [57..59)
       |Ident(b) [60..61)
       |Indentation.Outdent [68..68)
       |RightBrace [69..70)
       |EOF [71..71)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.foreach { x => x match
       |  case 1 => a
       |  case _ => b
       |
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply xs.foreach { x => x match
       |  case 1 => a
       |  case _ => b
       |
       |  // c
       |} [8:xs.foreach { x => x match...:71)
       |Term.Select xs.foreach [8:xs.foreach:18)
       |Term.Block { x => x match
       |  case 1 => a
       |  case _ => b
       |
       |  // c
       |} [19:{ x => x match...:71)
       |Term.Function x => x match
       |  case 1 => a
       |  case _ => b
       |
       |  // c [21:x => x match...:69)
       |Term.ParamClause x [21:x:22)
       |Term.Param x [21:x:22)
       |Term.Match x match
       |  case 1 => a
       |  case _ => b
       |
       |  // c [26:x match...:69)
       |Term.CasesBlock case 1 => a
       |  case _ => b
       |
       |  // c [36:case 1 => a...:69)
       |Case case 1 => a [36:case 1 => a:47)
       |Case case _ => b [50:case _ => b:61)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(xs) [8..10)
       |Dot [10..11)
       |Ident(foreach) [11..18)
       |LeftBrace [19..20)
       |Ident(x) [21..22)
       |RightArrow [23..25)
       |Ident(x) [26..27)
       |KwMatch [28..33)
       |Indentation.Indent [33..33)
       |KwCase [36..40)
       |Constant.Int(1) [41..42)
       |RightArrow [43..45)
       |Ident(a) [46..47)
       |LF [47..48)
       |KwCase [50..54)
       |Underscore [55..56)
       |RightArrow [57..59)
       |Ident(b) [60..61)
       |Indentation.Outdent [69..69)
       |RightBrace [70..71)
       |EOF [72..72)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { implicit ec: Ec =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { implicit ec: Ec =>
       |  a
       |  // c
       |} [8:foo { implicit ec: Ec =>...:45)
       |Term.Block { implicit ec: Ec =>
       |  a
       |  // c
       |} [12:{ implicit ec: Ec =>...:45)
       |Term.Function implicit ec: Ec =>
       |  a
       |  // c [14:implicit ec: Ec =>...:43)
       |Term.ParamClause implicit ec: Ec [14:implicit ec: Ec:29)
       |Term.Param implicit ec: Ec [14:implicit ec: Ec:29)
       |Term.Block a
       |  // c [35:a...:43)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwImplicit [14..22)
       |Ident(ec) [23..25)
       |Colon [25..26)
       |Ident(Ec) [27..29)
       |RightArrow [30..32)
       |Indentation.Indent [32..32)
       |Ident(a) [35..36)
       |Indentation.Outdent [43..43)
       |RightBrace [44..45)
       |EOF [46..46)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = bar(foo { implicit ec: Ec =>
       |  a
       |  // c
       |})
       |""".stripMargin,
    """|Term.Apply bar(foo { implicit ec: Ec =>
       |  a
       |  // c
       |}) [8:bar(foo { implicit ec: Ec =>...:50)
       |Term.ArgClause (foo { implicit ec: Ec =>
       |  a
       |  // c
       |}) [11:(foo { implicit ec: Ec =>...:50)
       |Term.Apply foo { implicit ec: Ec =>
       |  a
       |  // c
       |} [12:foo { implicit ec: Ec =>...:49)
       |Term.Block { implicit ec: Ec =>
       |  a
       |  // c
       |} [16:{ implicit ec: Ec =>...:49)
       |Term.Function implicit ec: Ec =>
       |  a
       |  // c [18:implicit ec: Ec =>...:47)
       |Term.ParamClause implicit ec: Ec [18:implicit ec: Ec:33)
       |Term.Param implicit ec: Ec [18:implicit ec: Ec:33)
       |Term.Block a
       |  // c [39:a...:47)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(bar) [8..11)
       |LeftParen [11..12)
       |Ident(foo) [12..15)
       |LeftBrace [16..17)
       |KwImplicit [18..26)
       |Ident(ec) [27..29)
       |Colon [29..30)
       |Ident(Ec) [31..33)
       |RightArrow [34..36)
       |Indentation.Indent [36..36)
       |Ident(a) [39..40)
       |Indentation.Outdent [47..47)
       |RightBrace [48..49)
       |RightParen [49..50)
       |EOF [51..51)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|trait B { self: C =>
       |  def f = 1
       |  // c
       |}
       |""".stripMargin,
    """|Type.ParamClause trait B @@{ self: C => [8::8)
       |Ctor.Primary trait B @@{ self: C => [8::8)
       |Template { self: C =>
       |  def f = 1
       |  // c
       |} [8:{ self: C =>...:41)
       |Template.Body { self: C =>
       |  def f = 1
       |  // c
       |} [8:{ self: C =>...:41)
       |Self self: C => [10:self: C =>:20)
       |Defn.Def def f = 1 [23:def f = 1:32)
       |""".stripMargin,
    """|BOF [0..0)
       |KwTrait [0..5)
       |Ident(B) [6..7)
       |LeftBrace [8..9)
       |Ident(self) [10..14)
       |Colon [14..15)
       |Ident(C) [16..17)
       |RightArrow [18..20)
       |Indentation.Indent [20..20)
       |KwDef [23..26)
       |Ident(f) [27..28)
       |Equals [29..30)
       |Constant.Int(1) [31..32)
       |Indentation.Outdent [39..39)
       |RightBrace [40..41)
       |EOF [42..42)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|trait D { this: C =>
       |  def f = 1
       |
       |  // c
       |}
       |""".stripMargin,
    """|Type.ParamClause trait D @@{ this: C => [8::8)
       |Ctor.Primary trait D @@{ this: C => [8::8)
       |Template { this: C =>
       |  def f = 1
       |
       |  // c
       |} [8:{ this: C =>...:42)
       |Template.Body { this: C =>
       |  def f = 1
       |
       |  // c
       |} [8:{ this: C =>...:42)
       |Self this: C => [10:this: C =>:20)
       |Defn.Def def f = 1 [23:def f = 1:32)
       |""".stripMargin,
    """|BOF [0..0)
       |KwTrait [0..5)
       |Ident(D) [6..7)
       |LeftBrace [8..9)
       |KwThis [10..14)
       |Colon [14..15)
       |Ident(C) [16..17)
       |RightArrow [18..20)
       |Indentation.Indent [20..20)
       |KwDef [23..26)
       |Ident(f) [27..28)
       |Equals [29..30)
       |Constant.Int(1) [31..32)
       |Indentation.Outdent [40..40)
       |RightBrace [41..42)
       |EOF [43..43)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo(new B { self: C =>
       |  def h = 1
       |  // c
       |})
       |""".stripMargin,
    """|Term.Apply foo(new B { self: C =>
       |  def h = 1
       |  // c
       |}) [8:foo(new B { self: C =>...:52)
       |Term.ArgClause (new B { self: C =>
       |  def h = 1
       |  // c
       |}) [11:(new B { self: C =>...:52)
       |Term.NewAnonymous new B { self: C =>
       |  def h = 1
       |  // c
       |} [12:new B { self: C =>...:51)
       |Template B { self: C =>
       |  def h = 1
       |  // c
       |} [16:B { self: C =>...:51)
       |Template.Body { self: C =>
       |  def h = 1
       |  // c
       |} [18:{ self: C =>...:51)
       |Self self: C => [20:self: C =>:30)
       |Defn.Def def h = 1 [33:def h = 1:42)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftParen [11..12)
       |KwNew [12..15)
       |Ident(B) [16..17)
       |LeftBrace [18..19)
       |Ident(self) [20..24)
       |Colon [24..25)
       |Ident(C) [26..27)
       |RightArrow [28..30)
       |Indentation.Indent [30..30)
       |KwDef [33..36)
       |Ident(h) [37..38)
       |Equals [39..40)
       |Constant.Int(1) [41..42)
       |Indentation.Outdent [49..49)
       |RightBrace [50..51)
       |RightParen [51..52)
       |EOF [53..53)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { implicit ec =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { implicit ec =>
       |  a
       |  // c
       |} [8:foo { implicit ec =>...:41)
       |Term.Block { implicit ec =>
       |  a
       |  // c
       |} [12:{ implicit ec =>...:41)
       |Term.Function implicit ec =>
       |  a
       |  // c [14:implicit ec =>...:39)
       |Term.ParamClause implicit ec [14:implicit ec:25)
       |Term.Param implicit ec [14:implicit ec:25)
       |Term.Block a
       |  // c [31:a...:39)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwImplicit [14..22)
       |Ident(ec) [23..25)
       |RightArrow [26..28)
       |Indentation.Indent [28..28)
       |Ident(a) [31..32)
       |Indentation.Outdent [39..39)
       |RightBrace [40..41)
       |EOF [42..42)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = { a
       |  b
       |
       |  // c
       |}
       |""".stripMargin,
    """|Term.Block { a
       |  b
       |
       |  // c
       |} [8:{ a...:25)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |LeftBrace [8..9)
       |Ident(a) [10..11)
       |LF [11..12)
       |Ident(b) [14..15)
       |LFLF [15..24)
       |RightBrace [24..25)
       |EOF [26..26)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { implicit request: Request[?] =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { implicit request: Request[?] =>
       |  a
       |  // c
       |} [8:foo { implicit request: Request[?] =>...:58)
       |Term.Block { implicit request: Request[?] =>
       |  a
       |  // c
       |} [12:{ implicit request: Request[?] =>...:58)
       |Term.Function implicit request: Request[?] =>
       |  a
       |  // c [14:implicit request: Request[?] =>...:56)
       |Term.ParamClause implicit request: Request[?] [14:implicit request: Request[?]:42)
       |Term.Param implicit request: Request[?] [14:implicit request: Request[?]:42)
       |Type.Apply Request[?] [32:Request[?]:42)
       |Type.ArgClause [?] [39:[?]:42)
       |Type.Wildcard ? [40:?:41)
       |Type.Bounds def f = foo { implicit request: Request[?@@] => [41::41)
       |Term.Block a
       |  // c [48:a...:56)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwImplicit [14..22)
       |Ident(request) [23..30)
       |Colon [30..31)
       |Ident(Request) [32..39)
       |LeftBracket [39..40)
       |Ident(?) [40..41)
       |RightBracket [41..42)
       |RightArrow [43..45)
       |Indentation.Indent [45..45)
       |Ident(a) [48..49)
       |Indentation.Outdent [56..56)
       |RightBrace [57..58)
       |EOF [59..59)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.foldLeft(0) { (acc, x) =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply xs.foldLeft(0) { (acc, x) =>
       |  a
       |  // c
       |} [8:xs.foldLeft(0) { (acc, x) =>...:49)
       |Term.Apply xs.foldLeft(0) [8:xs.foldLeft(0):22)
       |Term.Select xs.foldLeft [8:xs.foldLeft:19)
       |Term.ArgClause (0) [19:(0):22)
       |Term.Block { (acc, x) =>
       |  a
       |  // c
       |} [23:{ (acc, x) =>...:49)
       |Term.Function (acc, x) =>
       |  a
       |  // c [25:(acc, x) =>...:47)
       |Term.ParamClause (acc, x) [25:(acc, x):33)
       |Term.Param acc [26:acc:29)
       |Term.Param x [31:x:32)
       |Term.Block a
       |  // c [39:a...:47)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(xs) [8..10)
       |Dot [10..11)
       |Ident(foldLeft) [11..19)
       |LeftParen [19..20)
       |Constant.Int(0) [20..21)
       |RightParen [21..22)
       |LeftBrace [23..24)
       |LeftParen [25..26)
       |Ident(acc) [26..29)
       |Comma [29..30)
       |Ident(x) [31..32)
       |RightParen [32..33)
       |RightArrow [34..36)
       |Indentation.Indent [36..36)
       |Ident(a) [39..40)
       |Indentation.Outdent [47..47)
       |RightBrace [48..49)
       |EOF [50..50)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.foldLeft(0) { (acc,
       |    x) =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply xs.foldLeft(0) { (acc,
       |    x) =>
       |  a
       |  // c
       |} [8:xs.foldLeft(0) { (acc,...:53)
       |Term.Apply xs.foldLeft(0) [8:xs.foldLeft(0):22)
       |Term.Select xs.foldLeft [8:xs.foldLeft:19)
       |Term.ArgClause (0) [19:(0):22)
       |Term.Block { (acc,
       |    x) =>
       |  a
       |  // c
       |} [23:{ (acc,...:53)
       |Term.Function (acc,
       |    x) =>
       |  a
       |  // c [25:(acc,...:51)
       |Term.ParamClause (acc,
       |    x) [25:(acc,...:37)
       |Term.Param acc [26:acc:29)
       |Term.Param x [35:x:36)
       |Term.Block a
       |  // c [43:a...:51)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(xs) [8..10)
       |Dot [10..11)
       |Ident(foldLeft) [11..19)
       |LeftParen [19..20)
       |Constant.Int(0) [20..21)
       |RightParen [21..22)
       |LeftBrace [23..24)
       |LeftParen [25..26)
       |Ident(acc) [26..29)
       |Comma [29..30)
       |Ident(x) [35..36)
       |RightParen [36..37)
       |RightArrow [38..40)
       |Indentation.Indent [40..40)
       |Ident(a) [43..44)
       |Indentation.Outdent [51..51)
       |RightBrace [52..53)
       |EOF [54..54)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|trait B { self: C & D =>
       |  def f = 1
       |  // c
       |}
       |""".stripMargin,
    """|Type.ParamClause trait B @@{ self: C & D => [8::8)
       |Ctor.Primary trait B @@{ self: C & D => [8::8)
       |Template { self: C & D =>
       |  def f = 1
       |  // c
       |} [8:{ self: C & D =>...:45)
       |Template.Body { self: C & D =>
       |  def f = 1
       |  // c
       |} [8:{ self: C & D =>...:45)
       |Self self: C & D => [10:self: C & D =>:24)
       |Type.ApplyInfix C & D [16:C & D:21)
       |Defn.Def def f = 1 [27:def f = 1:36)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwTrait [0..5)
       |Ident(B) [6..7)
       |LeftBrace [8..9)
       |Ident(self) [10..14)
       |Colon [14..15)
       |Ident(C) [16..17)
       |Ident(&) [18..19)
       |Ident(D) [20..21)
       |RightArrow [22..24)
       |Indentation.Indent [24..24)
       |KwDef [27..30)
       |Ident(f) [31..32)
       |Equals [33..34)
       |Constant.Int(1) [35..36)
       |Indentation.Outdent [43..43)
       |RightBrace [44..45)
       |EOF [46..46)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|trait B { self: C[Int] =>
       |  def f = 1
       |  // c
       |}
       |""".stripMargin,
    """|Type.ParamClause trait B @@{ self: C[Int] => [8::8)
       |Ctor.Primary trait B @@{ self: C[Int] => [8::8)
       |Template { self: C[Int] =>
       |  def f = 1
       |  // c
       |} [8:{ self: C[Int] =>...:46)
       |Template.Body { self: C[Int] =>
       |  def f = 1
       |  // c
       |} [8:{ self: C[Int] =>...:46)
       |Self self: C[Int] => [10:self: C[Int] =>:25)
       |Type.Apply C[Int] [16:C[Int]:22)
       |Type.ArgClause [Int] [17:[Int]:22)
       |Defn.Def def f = 1 [28:def f = 1:37)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwTrait [0..5)
       |Ident(B) [6..7)
       |LeftBrace [8..9)
       |Ident(self) [10..14)
       |Colon [14..15)
       |Ident(C) [16..17)
       |LeftBracket [17..18)
       |Ident(Int) [18..21)
       |RightBracket [21..22)
       |RightArrow [23..25)
       |Indentation.Indent [25..25)
       |KwDef [28..31)
       |Ident(f) [32..33)
       |Equals [34..35)
       |Constant.Int(1) [36..37)
       |Indentation.Outdent [44..44)
       |RightBrace [45..46)
       |EOF [47..47)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = new B { self: C =>
       |  def h = 1
       |  // c
       |}
       |""".stripMargin,
    """|Term.NewAnonymous new B { self: C =>
       |  def h = 1
       |  // c
       |} [8:new B { self: C =>...:47)
       |Template B { self: C =>
       |  def h = 1
       |  // c
       |} [12:B { self: C =>...:47)
       |Template.Body { self: C =>
       |  def h = 1
       |  // c
       |} [14:{ self: C =>...:47)
       |Self self: C => [16:self: C =>:26)
       |Defn.Def def h = 1 [29:def h = 1:38)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |KwNew [8..11)
       |Ident(B) [12..13)
       |LeftBrace [14..15)
       |Ident(self) [16..20)
       |Colon [20..21)
       |Ident(C) [22..23)
       |RightArrow [24..26)
       |Indentation.Indent [26..26)
       |KwDef [29..32)
       |Ident(h) [33..34)
       |Equals [35..36)
       |Constant.Int(1) [37..38)
       |Indentation.Outdent [45..45)
       |RightBrace [46..47)
       |EOF [48..48)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { x => y =>
       |  a
       |}
       |""".stripMargin,
    """|Term.Apply foo { x => y =>
       |  a
       |} [8:foo { x => y =>...:29)
       |Term.Block { x => y =>
       |  a
       |} [12:{ x => y =>...:29)
       |Term.Function x => y =>
       |  a [14:x => y =>...:27)
       |Term.ParamClause x [14:x:15)
       |Term.Param x [14:x:15)
       |Term.Function y =>
       |  a [19:y =>...:27)
       |Term.ParamClause y [19:y:20)
       |Term.Param y [19:y:20)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |Ident(x) [14..15)
       |RightArrow [16..18)
       |Ident(y) [19..20)
       |RightArrow [21..23)
       |Indentation.Indent [23..23)
       |Ident(a) [26..27)
       |Indentation.Outdent [27..27)
       |LF [27..28)
       |RightBrace [28..29)
       |EOF [30..30)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { x => y =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { x => y =>
       |  a
       |  // c
       |} [8:foo { x => y =>...:36)
       |Term.Block { x => y =>
       |  a
       |  // c
       |} [12:{ x => y =>...:36)
       |Term.Function x => y =>
       |  a
       |  // c [14:x => y =>...:34)
       |Term.ParamClause x [14:x:15)
       |Term.Param x [14:x:15)
       |Term.Function y =>
       |  a
       |  // c [19:y =>...:34)
       |Term.ParamClause y [19:y:20)
       |Term.Param y [19:y:20)
       |Term.Block a
       |  // c [26:a...:34)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |Ident(x) [14..15)
       |RightArrow [16..18)
       |Ident(y) [19..20)
       |RightArrow [21..23)
       |Indentation.Indent [23..23)
       |Ident(a) [26..27)
       |Indentation.Outdent [34..34)
       |RightBrace [35..36)
       |EOF [37..37)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { x => y =>
       |  a
       |
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { x => y =>
       |  a
       |
       |  // c
       |} [8:foo { x => y =>...:37)
       |Term.Block { x => y =>
       |  a
       |
       |  // c
       |} [12:{ x => y =>...:37)
       |Term.Function x => y =>
       |  a
       |
       |  // c [14:x => y =>...:35)
       |Term.ParamClause x [14:x:15)
       |Term.Param x [14:x:15)
       |Term.Function y =>
       |  a
       |
       |  // c [19:y =>...:35)
       |Term.ParamClause y [19:y:20)
       |Term.Param y [19:y:20)
       |Term.Block a
       |
       |  // c [26:a...:35)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |Ident(x) [14..15)
       |RightArrow [16..18)
       |Ident(y) [19..20)
       |RightArrow [21..23)
       |Indentation.Indent [23..23)
       |Ident(a) [26..27)
       |Indentation.Outdent [35..35)
       |RightBrace [36..37)
       |EOF [38..38)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { x => y ?=>
       |  a
       |}
       |""".stripMargin,
    """|Term.Apply foo { x => y ?=>
       |  a
       |} [8:foo { x => y ?=>...:30)
       |Term.Block { x => y ?=>
       |  a
       |} [12:{ x => y ?=>...:30)
       |Term.Function x => y ?=>
       |  a [14:x => y ?=>...:28)
       |Term.ParamClause x [14:x:15)
       |Term.Param x [14:x:15)
       |Term.ContextFunction y ?=>
       |  a [19:y ?=>...:28)
       |Term.ParamClause y [19:y:20)
       |Term.Param y [19:y:20)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |Ident(x) [14..15)
       |RightArrow [16..18)
       |Ident(y) [19..20)
       |ContextArrow [21..24)
       |Indentation.Indent [24..24)
       |Ident(a) [27..28)
       |Indentation.Outdent [28..28)
       |LF [28..29)
       |RightBrace [29..30)
       |EOF [31..31)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { x => y ?=>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { x => y ?=>
       |  a
       |  // c
       |} [8:foo { x => y ?=>...:37)
       |Term.Block { x => y ?=>
       |  a
       |  // c
       |} [12:{ x => y ?=>...:37)
       |Term.Function x => y ?=>
       |  a
       |  // c [14:x => y ?=>...:35)
       |Term.ParamClause x [14:x:15)
       |Term.Param x [14:x:15)
       |Term.ContextFunction y ?=>
       |  a
       |  // c [19:y ?=>...:35)
       |Term.ParamClause y [19:y:20)
       |Term.Param y [19:y:20)
       |Term.Block a
       |  // c [27:a...:35)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |Ident(x) [14..15)
       |RightArrow [16..18)
       |Ident(y) [19..20)
       |ContextArrow [21..24)
       |Indentation.Indent [24..24)
       |Ident(a) [27..28)
       |Indentation.Outdent [35..35)
       |RightBrace [36..37)
       |EOF [38..38)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { x => (y: Int) =>
       |  a
       |}
       |""".stripMargin,
    """|Term.Apply foo { x => (y: Int) =>
       |  a
       |} [8:foo { x => (y: Int) =>...:36)
       |Term.Block { x => (y: Int) =>
       |  a
       |} [12:{ x => (y: Int) =>...:36)
       |Term.Function x => (y: Int) =>
       |  a [14:x => (y: Int) =>...:34)
       |Term.ParamClause x [14:x:15)
       |Term.Param x [14:x:15)
       |Term.Function (y: Int) =>
       |  a [19:(y: Int) =>...:34)
       |Term.ParamClause (y: Int) [19:(y: Int):27)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |Ident(x) [14..15)
       |RightArrow [16..18)
       |LeftParen [19..20)
       |Ident(y) [20..21)
       |Colon [21..22)
       |Ident(Int) [23..26)
       |RightParen [26..27)
       |RightArrow [28..30)
       |Indentation.Indent [30..30)
       |Ident(a) [33..34)
       |Indentation.Outdent [34..34)
       |LF [34..35)
       |RightBrace [35..36)
       |EOF [37..37)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { x => (y: Int) =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { x => (y: Int) =>
       |  a
       |  // c
       |} [8:foo { x => (y: Int) =>...:43)
       |Term.Block { x => (y: Int) =>
       |  a
       |  // c
       |} [12:{ x => (y: Int) =>...:43)
       |Term.Function x => (y: Int) =>
       |  a
       |  // c [14:x => (y: Int) =>...:41)
       |Term.ParamClause x [14:x:15)
       |Term.Param x [14:x:15)
       |Term.Function (y: Int) =>
       |  a
       |  // c [19:(y: Int) =>...:41)
       |Term.ParamClause (y: Int) [19:(y: Int):27)
       |Term.Block a
       |  // c [33:a...:41)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |Ident(x) [14..15)
       |RightArrow [16..18)
       |LeftParen [19..20)
       |Ident(y) [20..21)
       |Colon [21..22)
       |Ident(Int) [23..26)
       |RightParen [26..27)
       |RightArrow [28..30)
       |Indentation.Indent [30..30)
       |Ident(a) [33..34)
       |Indentation.Outdent [41..41)
       |RightBrace [42..43)
       |EOF [44..44)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val add: Int => Int => Int = { x => y =>
       |  x + y
       |}
       |""".stripMargin,
    """|Type.Function Int => Int => Int [9:Int => Int => Int:26)
       |Type.FuncParamClause Int [9:Int:12)
       |Type.Function Int => Int [16:Int => Int:26)
       |Type.FuncParamClause Int [16:Int:19)
       |Term.Block { x => y =>
       |  x + y
       |} [29:{ x => y =>...:50)
       |Term.Function x => y =>
       |  x + y [31:x => y =>...:48)
       |Term.ParamClause x [31:x:32)
       |Term.Param x [31:x:32)
       |Term.Function y =>
       |  x + y [36:y =>...:48)
       |Term.ParamClause y [36:y:37)
       |Term.Param y [36:y:37)
       |Term.ApplyInfix x + y [43:x + y:48)
       |Type.ArgClause   x + @@y [47::47)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(add) [4..7)
       |Colon [7..8)
       |Ident(Int) [9..12)
       |RightArrow [13..15)
       |Ident(Int) [16..19)
       |RightArrow [20..22)
       |Ident(Int) [23..26)
       |Equals [27..28)
       |LeftBrace [29..30)
       |Ident(x) [31..32)
       |RightArrow [33..35)
       |Ident(y) [36..37)
       |RightArrow [38..40)
       |Indentation.Indent [40..40)
       |Ident(x) [43..44)
       |Ident(+) [45..46)
       |Ident(y) [47..48)
       |Indentation.Outdent [48..48)
       |LF [48..49)
       |RightBrace [49..50)
       |EOF [51..51)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val add: Int => Int => Int = { x => y =>
       |  x + y
       |  // c
       |}
       |""".stripMargin,
    """|Type.Function Int => Int => Int [9:Int => Int => Int:26)
       |Type.FuncParamClause Int [9:Int:12)
       |Type.Function Int => Int [16:Int => Int:26)
       |Type.FuncParamClause Int [16:Int:19)
       |Term.Block { x => y =>
       |  x + y
       |  // c
       |} [29:{ x => y =>...:57)
       |Term.Function x => y =>
       |  x + y
       |  // c [31:x => y =>...:55)
       |Term.ParamClause x [31:x:32)
       |Term.Param x [31:x:32)
       |Term.Function y =>
       |  x + y
       |  // c [36:y =>...:55)
       |Term.ParamClause y [36:y:37)
       |Term.Param y [36:y:37)
       |Term.Block x + y
       |  // c [43:x + y...:55)
       |Term.ApplyInfix x + y [43:x + y:48)
       |Type.ArgClause   x + @@y [47::47)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(add) [4..7)
       |Colon [7..8)
       |Ident(Int) [9..12)
       |RightArrow [13..15)
       |Ident(Int) [16..19)
       |RightArrow [20..22)
       |Ident(Int) [23..26)
       |Equals [27..28)
       |LeftBrace [29..30)
       |Ident(x) [31..32)
       |RightArrow [33..35)
       |Ident(y) [36..37)
       |RightArrow [38..40)
       |Indentation.Indent [40..40)
       |Ident(x) [43..44)
       |Ident(+) [45..46)
       |Ident(y) [47..48)
       |Indentation.Outdent [55..55)
       |RightBrace [56..57)
       |EOF [58..58)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val add: Int => Int ?=> Int = { x => y ?=>
       |  x + y
       |}
       |""".stripMargin,
    """|Type.Function Int => Int ?=> Int [9:Int => Int ?=> Int:27)
       |Type.FuncParamClause Int [9:Int:12)
       |Type.ContextFunction Int ?=> Int [16:Int ?=> Int:27)
       |Type.FuncParamClause Int [16:Int:19)
       |Term.Block { x => y ?=>
       |  x + y
       |} [30:{ x => y ?=>...:52)
       |Term.Function x => y ?=>
       |  x + y [32:x => y ?=>...:50)
       |Term.ParamClause x [32:x:33)
       |Term.Param x [32:x:33)
       |Term.ContextFunction y ?=>
       |  x + y [37:y ?=>...:50)
       |Term.ParamClause y [37:y:38)
       |Term.Param y [37:y:38)
       |Term.ApplyInfix x + y [45:x + y:50)
       |Type.ArgClause   x + @@y [49::49)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(add) [4..7)
       |Colon [7..8)
       |Ident(Int) [9..12)
       |RightArrow [13..15)
       |Ident(Int) [16..19)
       |ContextArrow [20..23)
       |Ident(Int) [24..27)
       |Equals [28..29)
       |LeftBrace [30..31)
       |Ident(x) [32..33)
       |RightArrow [34..36)
       |Ident(y) [37..38)
       |ContextArrow [39..42)
       |Indentation.Indent [42..42)
       |Ident(x) [45..46)
       |Ident(+) [47..48)
       |Ident(y) [49..50)
       |Indentation.Outdent [50..50)
       |LF [50..51)
       |RightBrace [51..52)
       |EOF [53..53)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val add: Int => Int ?=> Int = { x => y ?=>
       |  x + y
       |  // c
       |}
       |""".stripMargin,
    """|Type.Function Int => Int ?=> Int [9:Int => Int ?=> Int:27)
       |Type.FuncParamClause Int [9:Int:12)
       |Type.ContextFunction Int ?=> Int [16:Int ?=> Int:27)
       |Type.FuncParamClause Int [16:Int:19)
       |Term.Block { x => y ?=>
       |  x + y
       |  // c
       |} [30:{ x => y ?=>...:59)
       |Term.Function x => y ?=>
       |  x + y
       |  // c [32:x => y ?=>...:57)
       |Term.ParamClause x [32:x:33)
       |Term.Param x [32:x:33)
       |Term.ContextFunction y ?=>
       |  x + y
       |  // c [37:y ?=>...:57)
       |Term.ParamClause y [37:y:38)
       |Term.Param y [37:y:38)
       |Term.Block x + y
       |  // c [45:x + y...:57)
       |Term.ApplyInfix x + y [45:x + y:50)
       |Type.ArgClause   x + @@y [49::49)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(add) [4..7)
       |Colon [7..8)
       |Ident(Int) [9..12)
       |RightArrow [13..15)
       |Ident(Int) [16..19)
       |ContextArrow [20..23)
       |Ident(Int) [24..27)
       |Equals [28..29)
       |LeftBrace [30..31)
       |Ident(x) [32..33)
       |RightArrow [34..36)
       |Ident(y) [37..38)
       |ContextArrow [39..42)
       |Indentation.Indent [42..42)
       |Ident(x) [45..46)
       |Ident(+) [47..48)
       |Ident(y) [49..50)
       |Indentation.Outdent [57..57)
       |RightBrace [58..59)
       |EOF [60..60)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val add: Int => Int => Int = { x => (y: Int) =>
       |  x + y
       |}
       |""".stripMargin,
    """|Type.Function Int => Int => Int [9:Int => Int => Int:26)
       |Type.FuncParamClause Int [9:Int:12)
       |Type.Function Int => Int [16:Int => Int:26)
       |Type.FuncParamClause Int [16:Int:19)
       |Term.Block { x => (y: Int) =>
       |  x + y
       |} [29:{ x => (y: Int) =>...:57)
       |Term.Function x => (y: Int) =>
       |  x + y [31:x => (y: Int) =>...:55)
       |Term.ParamClause x [31:x:32)
       |Term.Param x [31:x:32)
       |Term.Function (y: Int) =>
       |  x + y [36:(y: Int) =>...:55)
       |Term.ParamClause (y: Int) [36:(y: Int):44)
       |Term.ApplyInfix x + y [50:x + y:55)
       |Type.ArgClause   x + @@y [54::54)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(add) [4..7)
       |Colon [7..8)
       |Ident(Int) [9..12)
       |RightArrow [13..15)
       |Ident(Int) [16..19)
       |RightArrow [20..22)
       |Ident(Int) [23..26)
       |Equals [27..28)
       |LeftBrace [29..30)
       |Ident(x) [31..32)
       |RightArrow [33..35)
       |LeftParen [36..37)
       |Ident(y) [37..38)
       |Colon [38..39)
       |Ident(Int) [40..43)
       |RightParen [43..44)
       |RightArrow [45..47)
       |Indentation.Indent [47..47)
       |Ident(x) [50..51)
       |Ident(+) [52..53)
       |Ident(y) [54..55)
       |Indentation.Outdent [55..55)
       |LF [55..56)
       |RightBrace [56..57)
       |EOF [58..58)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val add: Int => Int => Int = { x => (y: Int) =>
       |  x + y
       |  // c
       |}
       |""".stripMargin,
    """|Type.Function Int => Int => Int [9:Int => Int => Int:26)
       |Type.FuncParamClause Int [9:Int:12)
       |Type.Function Int => Int [16:Int => Int:26)
       |Type.FuncParamClause Int [16:Int:19)
       |Term.Block { x => (y: Int) =>
       |  x + y
       |  // c
       |} [29:{ x => (y: Int) =>...:64)
       |Term.Function x => (y: Int) =>
       |  x + y
       |  // c [31:x => (y: Int) =>...:62)
       |Term.ParamClause x [31:x:32)
       |Term.Param x [31:x:32)
       |Term.Function (y: Int) =>
       |  x + y
       |  // c [36:(y: Int) =>...:62)
       |Term.ParamClause (y: Int) [36:(y: Int):44)
       |Term.Block x + y
       |  // c [50:x + y...:62)
       |Term.ApplyInfix x + y [50:x + y:55)
       |Type.ArgClause   x + @@y [54::54)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(add) [4..7)
       |Colon [7..8)
       |Ident(Int) [9..12)
       |RightArrow [13..15)
       |Ident(Int) [16..19)
       |RightArrow [20..22)
       |Ident(Int) [23..26)
       |Equals [27..28)
       |LeftBrace [29..30)
       |Ident(x) [31..32)
       |RightArrow [33..35)
       |LeftParen [36..37)
       |Ident(y) [37..38)
       |Colon [38..39)
       |Ident(Int) [40..43)
       |RightParen [43..44)
       |RightArrow [45..47)
       |Indentation.Indent [47..47)
       |Ident(x) [50..51)
       |Ident(+) [52..53)
       |Ident(y) [54..55)
       |Indentation.Outdent [62..62)
       |RightBrace [63..64)
       |EOF [65..65)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { implicit ec => implicit mat =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { implicit ec => implicit mat =>
       |  a
       |  // c
       |} [8:foo { implicit ec => implicit mat =>...:57)
       |Term.Block { implicit ec => implicit mat =>
       |  a
       |  // c
       |} [12:{ implicit ec => implicit mat =>...:57)
       |Term.Function implicit ec => implicit mat =>
       |  a
       |  // c [14:implicit ec => implicit mat =>...:55)
       |Term.ParamClause implicit ec [14:implicit ec:25)
       |Term.Param implicit ec [14:implicit ec:25)
       |Term.Function implicit mat =>
       |  a
       |  // c [29:implicit mat =>...:55)
       |Term.ParamClause implicit mat [29:implicit mat:41)
       |Term.Param implicit mat [29:implicit mat:41)
       |Term.Block a
       |  // c [47:a...:55)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwImplicit [14..22)
       |Ident(ec) [23..25)
       |RightArrow [26..28)
       |KwImplicit [29..37)
       |Ident(mat) [38..41)
       |RightArrow [42..44)
       |Indentation.Indent [44..44)
       |Ident(a) [47..48)
       |Indentation.Outdent [55..55)
       |RightBrace [56..57)
       |EOF [58..58)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { x => y => z =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { x => y => z =>
       |  a
       |  // c
       |} [8:foo { x => y => z =>...:41)
       |Term.Block { x => y => z =>
       |  a
       |  // c
       |} [12:{ x => y => z =>...:41)
       |Term.Function x => y => z =>
       |  a
       |  // c [14:x => y => z =>...:39)
       |Term.ParamClause x [14:x:15)
       |Term.Param x [14:x:15)
       |Term.Function y => z =>
       |  a
       |  // c [19:y => z =>...:39)
       |Term.ParamClause y [19:y:20)
       |Term.Param y [19:y:20)
       |Term.Function z =>
       |  a
       |  // c [24:z =>...:39)
       |Term.ParamClause z [24:z:25)
       |Term.Param z [24:z:25)
       |Term.Block a
       |  // c [31:a...:39)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |Ident(x) [14..15)
       |RightArrow [16..18)
       |Ident(y) [19..20)
       |RightArrow [21..23)
       |Ident(z) [24..25)
       |RightArrow [26..28)
       |Indentation.Indent [28..28)
       |Ident(a) [31..32)
       |Indentation.Outdent [39..39)
       |RightBrace [40..41)
       |EOF [42..42)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { x => implicit y =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { x => implicit y =>
       |  a
       |  // c
       |} [8:foo { x => implicit y =>...:45)
       |Term.Block { x => implicit y =>
       |  a
       |  // c
       |} [12:{ x => implicit y =>...:45)
       |Term.Function x => implicit y =>
       |  a
       |  // c [14:x => implicit y =>...:43)
       |Term.ParamClause x [14:x:15)
       |Term.Param x [14:x:15)
       |Term.Function implicit y =>
       |  a
       |  // c [19:implicit y =>...:43)
       |Term.ParamClause implicit y [19:implicit y:29)
       |Term.Param implicit y [19:implicit y:29)
       |Term.Block a
       |  // c [35:a...:43)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |Ident(x) [14..15)
       |RightArrow [16..18)
       |KwImplicit [19..27)
       |Ident(y) [28..29)
       |RightArrow [30..32)
       |Indentation.Indent [32..32)
       |Ident(a) [35..36)
       |Indentation.Outdent [43..43)
       |RightBrace [44..45)
       |EOF [46..46)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { implicit x: Int => y =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { implicit x: Int => y =>
       |  a
       |  // c
       |} [8:foo { implicit x: Int => y =>...:50)
       |Term.Block { implicit x: Int => y =>
       |  a
       |  // c
       |} [12:{ implicit x: Int => y =>...:50)
       |Term.Function implicit x: Int => y =>
       |  a
       |  // c [14:implicit x: Int => y =>...:48)
       |Term.ParamClause implicit x: Int [14:implicit x: Int:29)
       |Term.Param implicit x: Int [14:implicit x: Int:29)
       |Term.Function y =>
       |  a
       |  // c [33:y =>...:48)
       |Term.ParamClause y [33:y:34)
       |Term.Param y [33:y:34)
       |Term.Block a
       |  // c [40:a...:48)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwImplicit [14..22)
       |Ident(x) [23..24)
       |Colon [24..25)
       |Ident(Int) [26..29)
       |RightArrow [30..32)
       |Ident(y) [33..34)
       |RightArrow [35..37)
       |Indentation.Indent [37..37)
       |Ident(a) [40..41)
       |Indentation.Outdent [48..48)
       |RightBrace [49..50)
       |EOF [51..51)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { (x, y) => z =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { (x, y) => z =>
       |  a
       |  // c
       |} [8:foo { (x, y) => z =>...:41)
       |Term.Block { (x, y) => z =>
       |  a
       |  // c
       |} [12:{ (x, y) => z =>...:41)
       |Term.Function (x, y) => z =>
       |  a
       |  // c [14:(x, y) => z =>...:39)
       |Term.ParamClause (x, y) [14:(x, y):20)
       |Term.Param x [15:x:16)
       |Term.Param y [18:y:19)
       |Term.Function z =>
       |  a
       |  // c [24:z =>...:39)
       |Term.ParamClause z [24:z:25)
       |Term.Param z [24:z:25)
       |Term.Block a
       |  // c [31:a...:39)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |LeftParen [14..15)
       |Ident(x) [15..16)
       |Comma [16..17)
       |Ident(y) [18..19)
       |RightParen [19..20)
       |RightArrow [21..23)
       |Ident(z) [24..25)
       |RightArrow [26..28)
       |Indentation.Indent [28..28)
       |Ident(a) [31..32)
       |Indentation.Outdent [39..39)
       |RightBrace [40..41)
       |EOF [42..42)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val f = { [T] => (x: T) =>
       |  x
       |  // c
       |}
       |""".stripMargin,
    """|Term.Block { [T] => (x: T) =>
       |  x
       |  // c
       |} [8:{ [T] => (x: T) =>...:39)
       |Term.PolyFunction [T] => (x: T) =>
       |  x
       |  // c [10:[T] => (x: T) =>...:37)
       |Type.ParamClause [T] [10:[T]:13)
       |Type.ParamClause val f = { [T@@] => (x: T) => [12::12)
       |Type.Bounds val f = { [T@@] => (x: T) => [12::12)
       |Term.Function (x: T) =>
       |  x
       |  // c [17:(x: T) =>...:37)
       |Term.ParamClause (x: T) [17:(x: T):23)
       |Term.Block x
       |  // c [29:x...:37)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |LeftBrace [8..9)
       |LeftBracket [10..11)
       |Ident(T) [11..12)
       |RightBracket [12..13)
       |RightArrow [14..16)
       |LeftParen [17..18)
       |Ident(x) [18..19)
       |Colon [19..20)
       |Ident(T) [21..22)
       |RightParen [22..23)
       |RightArrow [24..26)
       |Indentation.Indent [26..26)
       |Ident(x) [29..30)
       |Indentation.Outdent [37..37)
       |RightBrace [38..39)
       |EOF [40..40)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo({ x => y =>
       |  a
       |  // c
       |})
       |""".stripMargin,
    """|Term.Apply foo({ x => y =>
       |  a
       |  // c
       |}) [8:foo({ x => y =>...:37)
       |Term.ArgClause ({ x => y =>
       |  a
       |  // c
       |}) [11:({ x => y =>...:37)
       |Term.Block { x => y =>
       |  a
       |  // c
       |} [12:{ x => y =>...:36)
       |Term.Function x => y =>
       |  a
       |  // c [14:x => y =>...:34)
       |Term.ParamClause x [14:x:15)
       |Term.Param x [14:x:15)
       |Term.Function y =>
       |  a
       |  // c [19:y =>...:34)
       |Term.ParamClause y [19:y:20)
       |Term.Param y [19:y:20)
       |Term.Block a
       |  // c [26:a...:34)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftParen [11..12)
       |LeftBrace [12..13)
       |Ident(x) [14..15)
       |RightArrow [16..18)
       |Ident(y) [19..20)
       |RightArrow [21..23)
       |Indentation.Indent [23..23)
       |Ident(a) [26..27)
       |Indentation.Outdent [34..34)
       |RightBrace [35..36)
       |RightParen [36..37)
       |EOF [38..38)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { case x => y =>
       |  a
       |}
       |""".stripMargin,
    """|Term.Apply foo { case x => y =>
       |  a
       |} [8:foo { case x => y =>...:34)
       |Term.PartialFunction { case x => y =>
       |  a
       |} [12:{ case x => y =>...:34)
       |Case case x => y =>
       |  a [14:case x => y =>...:32)
       |Term.Function y =>
       |  a [24:y =>...:32)
       |Term.ParamClause y [24:y:25)
       |Term.Param y [24:y:25)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwCase [14..18)
       |Ident(x) [19..20)
       |RightArrow [21..23)
       |Ident(y) [24..25)
       |RightArrow [26..28)
       |Indentation.Indent [28..28)
       |Ident(a) [31..32)
       |Indentation.Outdent [32..32)
       |LF [32..33)
       |RightBrace [33..34)
       |EOF [35..35)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { case x => y =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { case x => y =>
       |  a
       |  // c
       |} [8:foo { case x => y =>...:41)
       |Term.PartialFunction { case x => y =>
       |  a
       |  // c
       |} [12:{ case x => y =>...:41)
       |Case case x => y =>
       |  a
       |  // c [14:case x => y =>...:39)
       |Term.Function y =>
       |  a
       |  // c [24:y =>...:39)
       |Term.ParamClause y [24:y:25)
       |Term.Param y [24:y:25)
       |Term.Block a
       |  // c [31:a...:39)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwCase [14..18)
       |Ident(x) [19..20)
       |RightArrow [21..23)
       |Ident(y) [24..25)
       |RightArrow [26..28)
       |Indentation.Indent [28..28)
       |Ident(a) [31..32)
       |Indentation.Outdent [39..39)
       |RightBrace [40..41)
       |EOF [42..42)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { case x => y ?=>
       |  a
       |}
       |""".stripMargin,
    """|Term.Apply foo { case x => y ?=>
       |  a
       |} [8:foo { case x => y ?=>...:35)
       |Term.PartialFunction { case x => y ?=>
       |  a
       |} [12:{ case x => y ?=>...:35)
       |Case case x => y ?=>
       |  a [14:case x => y ?=>...:33)
       |Term.ContextFunction y ?=>
       |  a [24:y ?=>...:33)
       |Term.ParamClause y [24:y:25)
       |Term.Param y [24:y:25)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwCase [14..18)
       |Ident(x) [19..20)
       |RightArrow [21..23)
       |Ident(y) [24..25)
       |ContextArrow [26..29)
       |Indentation.Indent [29..29)
       |Ident(a) [32..33)
       |Indentation.Outdent [33..33)
       |LF [33..34)
       |RightBrace [34..35)
       |EOF [36..36)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { case x => y ?=>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { case x => y ?=>
       |  a
       |  // c
       |} [8:foo { case x => y ?=>...:42)
       |Term.PartialFunction { case x => y ?=>
       |  a
       |  // c
       |} [12:{ case x => y ?=>...:42)
       |Case case x => y ?=>
       |  a
       |  // c [14:case x => y ?=>...:40)
       |Term.ContextFunction y ?=>
       |  a
       |  // c [24:y ?=>...:40)
       |Term.ParamClause y [24:y:25)
       |Term.Param y [24:y:25)
       |Term.Block a
       |  // c [32:a...:40)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwCase [14..18)
       |Ident(x) [19..20)
       |RightArrow [21..23)
       |Ident(y) [24..25)
       |ContextArrow [26..29)
       |Indentation.Indent [29..29)
       |Ident(a) [32..33)
       |Indentation.Outdent [40..40)
       |RightBrace [41..42)
       |EOF [43..43)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { case x => implicit y =>
       |  a
       |}
       |""".stripMargin,
    """|Term.Apply foo { case x => implicit y =>
       |  a
       |} [8:foo { case x => implicit y =>...:43)
       |Term.PartialFunction { case x => implicit y =>
       |  a
       |} [12:{ case x => implicit y =>...:43)
       |Case case x => implicit y =>
       |  a [14:case x => implicit y =>...:41)
       |Term.Function implicit y =>
       |  a [24:implicit y =>...:41)
       |Term.ParamClause implicit y [24:implicit y:34)
       |Term.Param implicit y [24:implicit y:34)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwCase [14..18)
       |Ident(x) [19..20)
       |RightArrow [21..23)
       |KwImplicit [24..32)
       |Ident(y) [33..34)
       |RightArrow [35..37)
       |Indentation.Indent [37..37)
       |Ident(a) [40..41)
       |Indentation.Outdent [41..41)
       |LF [41..42)
       |RightBrace [42..43)
       |EOF [44..44)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { case x => implicit y =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { case x => implicit y =>
       |  a
       |  // c
       |} [8:foo { case x => implicit y =>...:50)
       |Term.PartialFunction { case x => implicit y =>
       |  a
       |  // c
       |} [12:{ case x => implicit y =>...:50)
       |Case case x => implicit y =>
       |  a
       |  // c [14:case x => implicit y =>...:48)
       |Term.Function implicit y =>
       |  a
       |  // c [24:implicit y =>...:48)
       |Term.ParamClause implicit y [24:implicit y:34)
       |Term.Param implicit y [24:implicit y:34)
       |Term.Block a
       |  // c [40:a...:48)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwCase [14..18)
       |Ident(x) [19..20)
       |RightArrow [21..23)
       |KwImplicit [24..32)
       |Ident(y) [33..34)
       |RightArrow [35..37)
       |Indentation.Indent [37..37)
       |Ident(a) [40..41)
       |Indentation.Outdent [48..48)
       |RightBrace [49..50)
       |EOF [51..51)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { case x => y => z =>
       |  a
       |}
       |""".stripMargin,
    """|Term.Apply foo { case x => y => z =>
       |  a
       |} [8:foo { case x => y => z =>...:39)
       |Term.PartialFunction { case x => y => z =>
       |  a
       |} [12:{ case x => y => z =>...:39)
       |Case case x => y => z =>
       |  a [14:case x => y => z =>...:37)
       |Term.Function y => z =>
       |  a [24:y => z =>...:37)
       |Term.ParamClause y [24:y:25)
       |Term.Param y [24:y:25)
       |Term.Function z =>
       |  a [29:z =>...:37)
       |Term.ParamClause z [29:z:30)
       |Term.Param z [29:z:30)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwCase [14..18)
       |Ident(x) [19..20)
       |RightArrow [21..23)
       |Ident(y) [24..25)
       |RightArrow [26..28)
       |Ident(z) [29..30)
       |RightArrow [31..33)
       |Indentation.Indent [33..33)
       |Ident(a) [36..37)
       |Indentation.Outdent [37..37)
       |LF [37..38)
       |RightBrace [38..39)
       |EOF [40..40)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { case x => y => z =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { case x => y => z =>
       |  a
       |  // c
       |} [8:foo { case x => y => z =>...:46)
       |Term.PartialFunction { case x => y => z =>
       |  a
       |  // c
       |} [12:{ case x => y => z =>...:46)
       |Case case x => y => z =>
       |  a
       |  // c [14:case x => y => z =>...:44)
       |Term.Function y => z =>
       |  a
       |  // c [24:y => z =>...:44)
       |Term.ParamClause y [24:y:25)
       |Term.Param y [24:y:25)
       |Term.Function z =>
       |  a
       |  // c [29:z =>...:44)
       |Term.ParamClause z [29:z:30)
       |Term.Param z [29:z:30)
       |Term.Block a
       |  // c [36:a...:44)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwCase [14..18)
       |Ident(x) [19..20)
       |RightArrow [21..23)
       |Ident(y) [24..25)
       |RightArrow [26..28)
       |Ident(z) [29..30)
       |RightArrow [31..33)
       |Indentation.Indent [33..33)
       |Ident(a) [36..37)
       |Indentation.Outdent [44..44)
       |RightBrace [45..46)
       |EOF [47..47)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = xs.map { case (k, v) => implicit ctx =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply xs.map { case (k, v) => implicit ctx =>
       |  a
       |  // c
       |} [8:xs.map { case (k, v) => implicit ctx =>...:60)
       |Term.Select xs.map [8:xs.map:14)
       |Term.PartialFunction { case (k, v) => implicit ctx =>
       |  a
       |  // c
       |} [15:{ case (k, v) => implicit ctx =>...:60)
       |Case case (k, v) => implicit ctx =>
       |  a
       |  // c [17:case (k, v) => implicit ctx =>...:58)
       |Pat.Tuple (k, v) [22:(k, v):28)
       |Term.Function implicit ctx =>
       |  a
       |  // c [32:implicit ctx =>...:58)
       |Term.ParamClause implicit ctx [32:implicit ctx:44)
       |Term.Param implicit ctx [32:implicit ctx:44)
       |Term.Block a
       |  // c [50:a...:58)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(xs) [8..10)
       |Dot [10..11)
       |Ident(map) [11..14)
       |LeftBrace [15..16)
       |KwCase [17..21)
       |LeftParen [22..23)
       |Ident(k) [23..24)
       |Comma [24..25)
       |Ident(v) [26..27)
       |RightParen [27..28)
       |RightArrow [29..31)
       |KwImplicit [32..40)
       |Ident(ctx) [41..44)
       |RightArrow [45..47)
       |Indentation.Indent [47..47)
       |Ident(a) [50..51)
       |Indentation.Outdent [58..58)
       |RightBrace [59..60)
       |EOF [61..61)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = x match { case 1 => y =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Match x match { case 1 => y =>
       |  a
       |  // c
       |} [8:x match { case 1 => y =>...:45)
       |Term.CasesBlock { case 1 => y =>
       |  a
       |  // c
       |} [16:{ case 1 => y =>...:45)
       |Case case 1 => y =>
       |  a
       |  // c [18:case 1 => y =>...:43)
       |Term.Function y =>
       |  a
       |  // c [28:y =>...:43)
       |Term.ParamClause y [28:y:29)
       |Term.Param y [28:y:29)
       |Term.Block a
       |  // c [35:a...:43)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(x) [8..9)
       |KwMatch [10..15)
       |LeftBrace [16..17)
       |KwCase [18..22)
       |Constant.Int(1) [23..24)
       |RightArrow [25..27)
       |Ident(y) [28..29)
       |RightArrow [30..32)
       |Indentation.Indent [32..32)
       |Ident(a) [35..36)
       |Indentation.Outdent [43..43)
       |RightBrace [44..45)
       |EOF [46..46)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val add: Int => Int => Int = { case x => y =>
       |  x + y
       |}
       |""".stripMargin,
    """|Type.Function Int => Int => Int [9:Int => Int => Int:26)
       |Type.FuncParamClause Int [9:Int:12)
       |Type.Function Int => Int [16:Int => Int:26)
       |Type.FuncParamClause Int [16:Int:19)
       |Term.PartialFunction { case x => y =>
       |  x + y
       |} [29:{ case x => y =>...:55)
       |Case case x => y =>
       |  x + y [31:case x => y =>...:53)
       |Term.Function y =>
       |  x + y [41:y =>...:53)
       |Term.ParamClause y [41:y:42)
       |Term.Param y [41:y:42)
       |Term.ApplyInfix x + y [48:x + y:53)
       |Type.ArgClause   x + @@y [52::52)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(add) [4..7)
       |Colon [7..8)
       |Ident(Int) [9..12)
       |RightArrow [13..15)
       |Ident(Int) [16..19)
       |RightArrow [20..22)
       |Ident(Int) [23..26)
       |Equals [27..28)
       |LeftBrace [29..30)
       |KwCase [31..35)
       |Ident(x) [36..37)
       |RightArrow [38..40)
       |Ident(y) [41..42)
       |RightArrow [43..45)
       |Indentation.Indent [45..45)
       |Ident(x) [48..49)
       |Ident(+) [50..51)
       |Ident(y) [52..53)
       |Indentation.Outdent [53..53)
       |LF [53..54)
       |RightBrace [54..55)
       |EOF [56..56)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val add: Int => Int => Int = { case x => y =>
       |  x + y
       |  // c
       |}
       |""".stripMargin,
    """|Type.Function Int => Int => Int [9:Int => Int => Int:26)
       |Type.FuncParamClause Int [9:Int:12)
       |Type.Function Int => Int [16:Int => Int:26)
       |Type.FuncParamClause Int [16:Int:19)
       |Term.PartialFunction { case x => y =>
       |  x + y
       |  // c
       |} [29:{ case x => y =>...:62)
       |Case case x => y =>
       |  x + y
       |  // c [31:case x => y =>...:60)
       |Term.Function y =>
       |  x + y
       |  // c [41:y =>...:60)
       |Term.ParamClause y [41:y:42)
       |Term.Param y [41:y:42)
       |Term.Block x + y
       |  // c [48:x + y...:60)
       |Term.ApplyInfix x + y [48:x + y:53)
       |Type.ArgClause   x + @@y [52::52)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(add) [4..7)
       |Colon [7..8)
       |Ident(Int) [9..12)
       |RightArrow [13..15)
       |Ident(Int) [16..19)
       |RightArrow [20..22)
       |Ident(Int) [23..26)
       |Equals [27..28)
       |LeftBrace [29..30)
       |KwCase [31..35)
       |Ident(x) [36..37)
       |RightArrow [38..40)
       |Ident(y) [41..42)
       |RightArrow [43..45)
       |Indentation.Indent [45..45)
       |Ident(x) [48..49)
       |Ident(+) [50..51)
       |Ident(y) [52..53)
       |Indentation.Outdent [60..60)
       |RightBrace [61..62)
       |EOF [63..63)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val add: Int => Int ?=> Int = { case x => y ?=>
       |  x + y
       |}
       |""".stripMargin,
    """|Type.Function Int => Int ?=> Int [9:Int => Int ?=> Int:27)
       |Type.FuncParamClause Int [9:Int:12)
       |Type.ContextFunction Int ?=> Int [16:Int ?=> Int:27)
       |Type.FuncParamClause Int [16:Int:19)
       |Term.PartialFunction { case x => y ?=>
       |  x + y
       |} [30:{ case x => y ?=>...:57)
       |Case case x => y ?=>
       |  x + y [32:case x => y ?=>...:55)
       |Term.ContextFunction y ?=>
       |  x + y [42:y ?=>...:55)
       |Term.ParamClause y [42:y:43)
       |Term.Param y [42:y:43)
       |Term.ApplyInfix x + y [50:x + y:55)
       |Type.ArgClause   x + @@y [54::54)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(add) [4..7)
       |Colon [7..8)
       |Ident(Int) [9..12)
       |RightArrow [13..15)
       |Ident(Int) [16..19)
       |ContextArrow [20..23)
       |Ident(Int) [24..27)
       |Equals [28..29)
       |LeftBrace [30..31)
       |KwCase [32..36)
       |Ident(x) [37..38)
       |RightArrow [39..41)
       |Ident(y) [42..43)
       |ContextArrow [44..47)
       |Indentation.Indent [47..47)
       |Ident(x) [50..51)
       |Ident(+) [52..53)
       |Ident(y) [54..55)
       |Indentation.Outdent [55..55)
       |LF [55..56)
       |RightBrace [56..57)
       |EOF [58..58)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val add: Int => Int ?=> Int = { case x => y ?=>
       |  x + y
       |  // c
       |}
       |""".stripMargin,
    """|Type.Function Int => Int ?=> Int [9:Int => Int ?=> Int:27)
       |Type.FuncParamClause Int [9:Int:12)
       |Type.ContextFunction Int ?=> Int [16:Int ?=> Int:27)
       |Type.FuncParamClause Int [16:Int:19)
       |Term.PartialFunction { case x => y ?=>
       |  x + y
       |  // c
       |} [30:{ case x => y ?=>...:64)
       |Case case x => y ?=>
       |  x + y
       |  // c [32:case x => y ?=>...:62)
       |Term.ContextFunction y ?=>
       |  x + y
       |  // c [42:y ?=>...:62)
       |Term.ParamClause y [42:y:43)
       |Term.Param y [42:y:43)
       |Term.Block x + y
       |  // c [50:x + y...:62)
       |Term.ApplyInfix x + y [50:x + y:55)
       |Type.ArgClause   x + @@y [54::54)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(add) [4..7)
       |Colon [7..8)
       |Ident(Int) [9..12)
       |RightArrow [13..15)
       |Ident(Int) [16..19)
       |ContextArrow [20..23)
       |Ident(Int) [24..27)
       |Equals [28..29)
       |LeftBrace [30..31)
       |KwCase [32..36)
       |Ident(x) [37..38)
       |RightArrow [39..41)
       |Ident(y) [42..43)
       |ContextArrow [44..47)
       |Indentation.Indent [47..47)
       |Ident(x) [50..51)
       |Ident(+) [52..53)
       |Ident(y) [54..55)
       |Indentation.Outdent [62..62)
       |RightBrace [63..64)
       |EOF [65..65)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val add: Int => Int => Int = { case x => implicit y =>
       |  x + y
       |}
       |""".stripMargin,
    """|Type.Function Int => Int => Int [9:Int => Int => Int:26)
       |Type.FuncParamClause Int [9:Int:12)
       |Type.Function Int => Int [16:Int => Int:26)
       |Type.FuncParamClause Int [16:Int:19)
       |Term.PartialFunction { case x => implicit y =>
       |  x + y
       |} [29:{ case x => implicit y =>...:64)
       |Case case x => implicit y =>
       |  x + y [31:case x => implicit y =>...:62)
       |Term.Function implicit y =>
       |  x + y [41:implicit y =>...:62)
       |Term.ParamClause implicit y [41:implicit y:51)
       |Term.Param implicit y [41:implicit y:51)
       |Term.ApplyInfix x + y [57:x + y:62)
       |Type.ArgClause   x + @@y [61::61)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(add) [4..7)
       |Colon [7..8)
       |Ident(Int) [9..12)
       |RightArrow [13..15)
       |Ident(Int) [16..19)
       |RightArrow [20..22)
       |Ident(Int) [23..26)
       |Equals [27..28)
       |LeftBrace [29..30)
       |KwCase [31..35)
       |Ident(x) [36..37)
       |RightArrow [38..40)
       |KwImplicit [41..49)
       |Ident(y) [50..51)
       |RightArrow [52..54)
       |Indentation.Indent [54..54)
       |Ident(x) [57..58)
       |Ident(+) [59..60)
       |Ident(y) [61..62)
       |Indentation.Outdent [62..62)
       |LF [62..63)
       |RightBrace [63..64)
       |EOF [65..65)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val add: Int => Int => Int = { case x => implicit y =>
       |  x + y
       |  // c
       |}
       |""".stripMargin,
    """|Type.Function Int => Int => Int [9:Int => Int => Int:26)
       |Type.FuncParamClause Int [9:Int:12)
       |Type.Function Int => Int [16:Int => Int:26)
       |Type.FuncParamClause Int [16:Int:19)
       |Term.PartialFunction { case x => implicit y =>
       |  x + y
       |  // c
       |} [29:{ case x => implicit y =>...:71)
       |Case case x => implicit y =>
       |  x + y
       |  // c [31:case x => implicit y =>...:69)
       |Term.Function implicit y =>
       |  x + y
       |  // c [41:implicit y =>...:69)
       |Term.ParamClause implicit y [41:implicit y:51)
       |Term.Param implicit y [41:implicit y:51)
       |Term.Block x + y
       |  // c [57:x + y...:69)
       |Term.ApplyInfix x + y [57:x + y:62)
       |Type.ArgClause   x + @@y [61::61)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(add) [4..7)
       |Colon [7..8)
       |Ident(Int) [9..12)
       |RightArrow [13..15)
       |Ident(Int) [16..19)
       |RightArrow [20..22)
       |Ident(Int) [23..26)
       |Equals [27..28)
       |LeftBrace [29..30)
       |KwCase [31..35)
       |Ident(x) [36..37)
       |RightArrow [38..40)
       |KwImplicit [41..49)
       |Ident(y) [50..51)
       |RightArrow [52..54)
       |Indentation.Indent [54..54)
       |Ident(x) [57..58)
       |Ident(+) [59..60)
       |Ident(y) [61..62)
       |Indentation.Outdent [69..69)
       |RightBrace [70..71)
       |EOF [72..72)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val add: Int => Int => Int => Int = { case x => y => z =>
       |  x + y
       |}
       |""".stripMargin,
    """|Type.Function Int => Int => Int => Int [9:Int => Int => Int => Int:33)
       |Type.FuncParamClause Int [9:Int:12)
       |Type.Function Int => Int => Int [16:Int => Int => Int:33)
       |Type.FuncParamClause Int [16:Int:19)
       |Type.Function Int => Int [23:Int => Int:33)
       |Type.FuncParamClause Int [23:Int:26)
       |Term.PartialFunction { case x => y => z =>
       |  x + y
       |} [36:{ case x => y => z =>...:67)
       |Case case x => y => z =>
       |  x + y [38:case x => y => z =>...:65)
       |Term.Function y => z =>
       |  x + y [48:y => z =>...:65)
       |Term.ParamClause y [48:y:49)
       |Term.Param y [48:y:49)
       |Term.Function z =>
       |  x + y [53:z =>...:65)
       |Term.ParamClause z [53:z:54)
       |Term.Param z [53:z:54)
       |Term.ApplyInfix x + y [60:x + y:65)
       |Type.ArgClause   x + @@y [64::64)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(add) [4..7)
       |Colon [7..8)
       |Ident(Int) [9..12)
       |RightArrow [13..15)
       |Ident(Int) [16..19)
       |RightArrow [20..22)
       |Ident(Int) [23..26)
       |RightArrow [27..29)
       |Ident(Int) [30..33)
       |Equals [34..35)
       |LeftBrace [36..37)
       |KwCase [38..42)
       |Ident(x) [43..44)
       |RightArrow [45..47)
       |Ident(y) [48..49)
       |RightArrow [50..52)
       |Ident(z) [53..54)
       |RightArrow [55..57)
       |Indentation.Indent [57..57)
       |Ident(x) [60..61)
       |Ident(+) [62..63)
       |Ident(y) [64..65)
       |Indentation.Outdent [65..65)
       |LF [65..66)
       |RightBrace [66..67)
       |EOF [68..68)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|val add: Int => Int => Int => Int = { case x => y => z =>
       |  x + y
       |  // c
       |}
       |""".stripMargin,
    """|Type.Function Int => Int => Int => Int [9:Int => Int => Int => Int:33)
       |Type.FuncParamClause Int [9:Int:12)
       |Type.Function Int => Int => Int [16:Int => Int => Int:33)
       |Type.FuncParamClause Int [16:Int:19)
       |Type.Function Int => Int [23:Int => Int:33)
       |Type.FuncParamClause Int [23:Int:26)
       |Term.PartialFunction { case x => y => z =>
       |  x + y
       |  // c
       |} [36:{ case x => y => z =>...:74)
       |Case case x => y => z =>
       |  x + y
       |  // c [38:case x => y => z =>...:72)
       |Term.Function y => z =>
       |  x + y
       |  // c [48:y => z =>...:72)
       |Term.ParamClause y [48:y:49)
       |Term.Param y [48:y:49)
       |Term.Function z =>
       |  x + y
       |  // c [53:z =>...:72)
       |Term.ParamClause z [53:z:54)
       |Term.Param z [53:z:54)
       |Term.Block x + y
       |  // c [60:x + y...:72)
       |Term.ApplyInfix x + y [60:x + y:65)
       |Type.ArgClause   x + @@y [64::64)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwVal [0..3)
       |Ident(add) [4..7)
       |Colon [7..8)
       |Ident(Int) [9..12)
       |RightArrow [13..15)
       |Ident(Int) [16..19)
       |RightArrow [20..22)
       |Ident(Int) [23..26)
       |RightArrow [27..29)
       |Ident(Int) [30..33)
       |Equals [34..35)
       |LeftBrace [36..37)
       |KwCase [38..42)
       |Ident(x) [43..44)
       |RightArrow [45..47)
       |Ident(y) [48..49)
       |RightArrow [50..52)
       |Ident(z) [53..54)
       |RightArrow [55..57)
       |Indentation.Indent [57..57)
       |Ident(x) [60..61)
       |Ident(+) [62..63)
       |Ident(y) [64..65)
       |Indentation.Outdent [72..72)
       |RightBrace [73..74)
       |EOF [75..75)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { case x if p => y =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { case x if p => y =>
       |  a
       |  // c
       |} [8:foo { case x if p => y =>...:46)
       |Term.PartialFunction { case x if p => y =>
       |  a
       |  // c
       |} [12:{ case x if p => y =>...:46)
       |Case case x if p => y =>
       |  a
       |  // c [14:case x if p => y =>...:44)
       |Term.Function y =>
       |  a
       |  // c [29:y =>...:44)
       |Term.ParamClause y [29:y:30)
       |Term.Param y [29:y:30)
       |Term.Block a
       |  // c [36:a...:44)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwCase [14..18)
       |Ident(x) [19..20)
       |KwIf [21..23)
       |Ident(p) [24..25)
       |RightArrow [26..28)
       |Ident(y) [29..30)
       |RightArrow [31..33)
       |Indentation.Indent [33..33)
       |Ident(a) [36..37)
       |Indentation.Outdent [44..44)
       |RightBrace [45..46)
       |EOF [47..47)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { case x if p(z => z) => y =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { case x if p(z => z) => y =>
       |  a
       |  // c
       |} [8:foo { case x if p(z => z) => y =>...:54)
       |Term.PartialFunction { case x if p(z => z) => y =>
       |  a
       |  // c
       |} [12:{ case x if p(z => z) => y =>...:54)
       |Case case x if p(z => z) => y =>
       |  a
       |  // c [14:case x if p(z => z) => y =>...:52)
       |Term.Apply p(z => z) [24:p(z => z):33)
       |Term.ArgClause (z => z) [25:(z => z):33)
       |Term.Function z => z [26:z => z:32)
       |Term.ParamClause z [26:z:27)
       |Term.Param z [26:z:27)
       |Term.Function y =>
       |  a
       |  // c [37:y =>...:52)
       |Term.ParamClause y [37:y:38)
       |Term.Param y [37:y:38)
       |Term.Block a
       |  // c [44:a...:52)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwCase [14..18)
       |Ident(x) [19..20)
       |KwIf [21..23)
       |Ident(p) [24..25)
       |LeftParen [25..26)
       |Ident(z) [26..27)
       |RightArrow [28..30)
       |Ident(z) [31..32)
       |RightParen [32..33)
       |RightArrow [34..36)
       |Ident(y) [37..38)
       |RightArrow [39..41)
       |Indentation.Indent [41..41)
       |Ident(a) [44..45)
       |Indentation.Outdent [52..52)
       |RightBrace [53..54)
       |EOF [55..55)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = try a catch { case e => y =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Try try a catch { case e => y =>
       |  a
       |  // c
       |} [8:try a catch { case e => y =>...:49)
       |Term.CasesBlock { case e => y =>
       |  a
       |  // c
       |} [20:{ case e => y =>...:49)
       |Case case e => y =>
       |  a
       |  // c [22:case e => y =>...:47)
       |Term.Function y =>
       |  a
       |  // c [32:y =>...:47)
       |Term.ParamClause y [32:y:33)
       |Term.Param y [32:y:33)
       |Term.Block a
       |  // c [39:a...:47)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |KwTry [8..11)
       |Ident(a) [12..13)
       |KwCatch [14..19)
       |LeftBrace [20..21)
       |KwCase [22..26)
       |Ident(e) [27..28)
       |RightArrow [29..31)
       |Ident(y) [32..33)
       |RightArrow [34..36)
       |Indentation.Indent [36..36)
       |Ident(a) [39..40)
       |Indentation.Outdent [47..47)
       |RightBrace [48..49)
       |EOF [50..50)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { case x => bar { case z => y =>
       |  a
       |  // c
       |}}
       |""".stripMargin,
    """|Term.Apply foo { case x => bar { case z => y =>
       |  a
       |  // c
       |}} [8:foo { case x => bar { case z => y =>...:58)
       |Term.PartialFunction { case x => bar { case z => y =>
       |  a
       |  // c
       |}} [12:{ case x => bar { case z => y =>...:58)
       |Case case x => bar { case z => y =>
       |  a
       |  // c
       |} [14:case x => bar { case z => y =>...:57)
       |Term.Apply bar { case z => y =>
       |  a
       |  // c
       |} [24:bar { case z => y =>...:57)
       |Term.PartialFunction { case z => y =>
       |  a
       |  // c
       |} [28:{ case z => y =>...:57)
       |Case case z => y =>
       |  a
       |  // c [30:case z => y =>...:55)
       |Term.Function y =>
       |  a
       |  // c [40:y =>...:55)
       |Term.ParamClause y [40:y:41)
       |Term.Param y [40:y:41)
       |Term.Block a
       |  // c [47:a...:55)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwCase [14..18)
       |Ident(x) [19..20)
       |RightArrow [21..23)
       |Ident(bar) [24..27)
       |LeftBrace [28..29)
       |KwCase [30..34)
       |Ident(z) [35..36)
       |RightArrow [37..39)
       |Ident(y) [40..41)
       |RightArrow [42..44)
       |Indentation.Indent [44..44)
       |Ident(a) [47..48)
       |Indentation.Outdent [55..55)
       |RightBrace [56..57)
       |RightBrace [57..58)
       |EOF [59..59)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { case x => implicit y: Int =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { case x => implicit y: Int =>
       |  a
       |  // c
       |} [8:foo { case x => implicit y: Int =>...:55)
       |Term.PartialFunction { case x => implicit y: Int =>
       |  a
       |  // c
       |} [12:{ case x => implicit y: Int =>...:55)
       |Case case x => implicit y: Int =>
       |  a
       |  // c [14:case x => implicit y: Int =>...:53)
       |Term.Function implicit y: Int =>
       |  a
       |  // c [24:implicit y: Int =>...:53)
       |Term.ParamClause implicit y: Int [24:implicit y: Int:39)
       |Term.Param implicit y: Int [24:implicit y: Int:39)
       |Term.Block a
       |  // c [45:a...:53)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwCase [14..18)
       |Ident(x) [19..20)
       |RightArrow [21..23)
       |KwImplicit [24..32)
       |Ident(y) [33..34)
       |Colon [34..35)
       |Ident(Int) [36..39)
       |RightArrow [40..42)
       |Indentation.Indent [42..42)
       |Ident(a) [45..46)
       |Indentation.Outdent [53..53)
       |RightBrace [54..55)
       |EOF [56..56)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { case 1 => a; case x => y =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { case 1 => a; case x => y =>
       |  a
       |  // c
       |} [8:foo { case 1 => a; case x => y =>...:54)
       |Term.PartialFunction { case 1 => a; case x => y =>
       |  a
       |  // c
       |} [12:{ case 1 => a; case x => y =>...:54)
       |Case case 1 => a; [14:case 1 => a;:26)
       |Case case x => y =>
       |  a
       |  // c [27:case x => y =>...:52)
       |Term.Function y =>
       |  a
       |  // c [37:y =>...:52)
       |Term.ParamClause y [37:y:38)
       |Term.Param y [37:y:38)
       |Term.Block a
       |  // c [44:a...:52)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |KwCase [14..18)
       |Constant.Int(1) [19..20)
       |RightArrow [21..23)
       |Ident(a) [24..25)
       |Semicolon [25..26)
       |KwCase [27..31)
       |Ident(x) [32..33)
       |RightArrow [34..36)
       |Ident(y) [37..38)
       |RightArrow [39..41)
       |Indentation.Indent [41..41)
       |Ident(a) [44..45)
       |Indentation.Outdent [52..52)
       |RightBrace [53..54)
       |EOF [55..55)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { x => implicit y: Int =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { x => implicit y: Int =>
       |  a
       |  // c
       |} [8:foo { x => implicit y: Int =>...:50)
       |Term.Block { x => implicit y: Int =>
       |  a
       |  // c
       |} [12:{ x => implicit y: Int =>...:50)
       |Term.Function x => implicit y: Int =>
       |  a
       |  // c [14:x => implicit y: Int =>...:48)
       |Term.ParamClause x [14:x:15)
       |Term.Param x [14:x:15)
       |Term.Function implicit y: Int =>
       |  a
       |  // c [19:implicit y: Int =>...:48)
       |Term.ParamClause implicit y: Int [19:implicit y: Int:34)
       |Term.Param implicit y: Int [19:implicit y: Int:34)
       |Term.Block a
       |  // c [40:a...:48)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |Ident(x) [14..15)
       |RightArrow [16..18)
       |KwImplicit [19..27)
       |Ident(y) [28..29)
       |Colon [29..30)
       |Ident(Int) [31..34)
       |RightArrow [35..37)
       |Indentation.Indent [37..37)
       |Ident(a) [40..41)
       |Indentation.Outdent [48..48)
       |RightBrace [49..50)
       |EOF [51..51)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { _ => y =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { _ => y =>
       |  a
       |  // c
       |} [8:foo { _ => y =>...:36)
       |Term.Block { _ => y =>
       |  a
       |  // c
       |} [12:{ _ => y =>...:36)
       |Term.Function _ => y =>
       |  a
       |  // c [14:_ => y =>...:34)
       |Term.ParamClause _ [14:_:15)
       |Term.Param _ [14:_:15)
       |Term.Function y =>
       |  a
       |  // c [19:y =>...:34)
       |Term.ParamClause y [19:y:20)
       |Term.Param y [19:y:20)
       |Term.Block a
       |  // c [26:a...:34)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |Underscore [14..15)
       |RightArrow [16..18)
       |Ident(y) [19..20)
       |RightArrow [21..23)
       |Indentation.Indent [23..23)
       |Ident(a) [26..27)
       |Indentation.Outdent [34..34)
       |RightBrace [35..36)
       |EOF [37..37)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { (x: Int) ?=> y =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { (x: Int) ?=> y =>
       |  a
       |  // c
       |} [8:foo { (x: Int) ?=> y =>...:44)
       |Term.Block { (x: Int) ?=> y =>
       |  a
       |  // c
       |} [12:{ (x: Int) ?=> y =>...:44)
       |Term.ContextFunction (x: Int) ?=> y =>
       |  a
       |  // c [14:(x: Int) ?=> y =>...:42)
       |Term.ParamClause (x: Int) [14:(x: Int):22)
       |Term.Function y =>
       |  a
       |  // c [27:y =>...:42)
       |Term.ParamClause y [27:y:28)
       |Term.Param y [27:y:28)
       |Term.Block a
       |  // c [34:a...:42)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |LeftParen [14..15)
       |Ident(x) [15..16)
       |Colon [16..17)
       |Ident(Int) [18..21)
       |RightParen [21..22)
       |ContextArrow [23..26)
       |Ident(y) [27..28)
       |RightArrow [29..31)
       |Indentation.Indent [31..31)
       |Ident(a) [34..35)
       |Indentation.Outdent [42..42)
       |RightBrace [43..44)
       |EOF [45..45)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = foo { /* p */ x => y =>
       |  a
       |  // c
       |}
       |""".stripMargin,
    """|Term.Apply foo { /* p */ x => y =>
       |  a
       |  // c
       |} [8:foo { /* p */ x => y =>...:44)
       |Term.Block { /* p */ x => y =>
       |  a
       |  // c
       |} [12:{ /* p */ x => y =>...:44)
       |Term.Function x => y =>
       |  a
       |  // c [22:x => y =>...:42)
       |Term.ParamClause x [22:x:23)
       |Term.Param x [22:x:23)
       |Term.Function y =>
       |  a
       |  // c [27:y =>...:42)
       |Term.ParamClause y [27:y:28)
       |Term.Param y [27:y:28)
       |Term.Block a
       |  // c [34:a...:42)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Ident(foo) [8..11)
       |LeftBrace [12..13)
       |Ident(x) [22..23)
       |RightArrow [24..26)
       |Ident(y) [27..28)
       |RightArrow [29..31)
       |Indentation.Indent [31..31)
       |Ident(a) [34..35)
       |Indentation.Outdent [42..42)
       |RightBrace [43..44)
       |EOF [45..45)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|trait B { this: C =>
       |  def f = 1
       |  // c
       |}
       |""".stripMargin,
    """|Type.ParamClause trait B @@{ this: C => [8::8)
       |Ctor.Primary trait B @@{ this: C => [8::8)
       |Template { this: C =>
       |  def f = 1
       |  // c
       |} [8:{ this: C =>...:41)
       |Template.Body { this: C =>
       |  def f = 1
       |  // c
       |} [8:{ this: C =>...:41)
       |Self this: C => [10:this: C =>:20)
       |Defn.Def def f = 1 [23:def f = 1:32)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwTrait [0..5)
       |Ident(B) [6..7)
       |LeftBrace [8..9)
       |KwThis [10..14)
       |Colon [14..15)
       |Ident(C) [16..17)
       |RightArrow [18..20)
       |Indentation.Indent [20..20)
       |KwDef [23..26)
       |Ident(f) [27..28)
       |Equals [29..30)
       |Constant.Int(1) [31..32)
       |Indentation.Outdent [39..39)
       |RightBrace [40..41)
       |EOF [42..42)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|trait B { self =>
       |  def f = 1
       |  // c
       |}
       |""".stripMargin,
    """|Type.ParamClause trait B @@{ self => [8::8)
       |Ctor.Primary trait B @@{ self => [8::8)
       |Template { self =>
       |  def f = 1
       |  // c
       |} [8:{ self =>...:38)
       |Template.Body { self =>
       |  def f = 1
       |  // c
       |} [8:{ self =>...:38)
       |Self self => [10:self =>:17)
       |Defn.Def def f = 1 [20:def f = 1:29)
       |
       |""".stripMargin,
    """|BOF [0..0)
       |KwTrait [0..5)
       |Ident(B) [6..7)
       |LeftBrace [8..9)
       |Ident(self) [10..14)
       |RightArrow [15..17)
       |Indentation.Indent [17..17)
       |KwDef [20..23)
       |Ident(f) [24..25)
       |Equals [26..27)
       |Constant.Int(1) [28..29)
       |Indentation.Outdent [36..36)
       |RightBrace [37..38)
       |EOF [39..39)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  throw
       |    new E
       |    // c
       |""".stripMargin,
    """|Term.Throw throw
       |    new E
       |    // c [10:throw...:34)
       |Term.Block new E
       |    // c [20:new E...:34)
       |Term.New new E [20:new E:25)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |KwThrow [10..15)
       |Indentation.Indent [15..15)
       |KwNew [20..23)
       |Ident(E) [24..25)
       |Indentation.Outdent [34..34)
       |Indentation.Outdent [34..34)
       |EOF [35..35)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  return
       |    new E
       |    // c
       |""".stripMargin,
    """|Term.Return return
       |    new E
       |    // c [10:return...:35)
       |Term.Block new E
       |    // c [21:new E...:35)
       |Term.New new E [21:new E:26)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |KwReturn [10..16)
       |Indentation.Indent [16..16)
       |KwNew [21..24)
       |Ident(E) [25..26)
       |Indentation.Outdent [35..35)
       |Indentation.Outdent [35..35)
       |EOF [36..36)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  g(xs.map: x =>
       |    x /* c */, 2)
       |""".stripMargin,
    """|Term.Apply g(xs.map: x =>
       |    x /* c */, 2) [10:g(xs.map: x =>...:42)
       |Term.ArgClause (xs.map: x =>
       |    x /* c */, 2) [11:(xs.map: x =>...:42)
       |Term.Apply xs.map: x =>
       |    x [12:xs.map: x =>...:30)
       |Term.Select xs.map [12:xs.map:18)
       |Term.Block : x =>
       |    x [18<: x =>...>30)
       |Term.Function x =>
       |    x [20:x =>...:30)
       |Term.ParamClause x [20:x:21)
       |Term.Param x [20:x:21)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |Ident(g) [10..11)
       |LeftParen [11..12)
       |Ident(xs) [12..14)
       |Dot [14..15)
       |Ident(map) [15..18)
       |Colon [18..19)
       |Ident(x) [20..21)
       |RightArrow [22..24)
       |Indentation.Indent [24..24)
       |Ident(x) [29..30)
       |Indentation.Outdent [38..38)
       |Comma [38..39)
       |Constant.Int(2) [40..41)
       |RightParen [41..42)
       |Indentation.Outdent [42..42)
       |EOF [43..43)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f =
       |  xs.map: x =>
       |    x // c""".stripMargin,
    """|Term.Apply xs.map: x =>
       |    x [10:xs.map: x =>...:28)
       |Term.Select xs.map [10:xs.map:16)
       |Term.Block : x =>
       |    x [16<: x =>...>28)
       |Term.Function x =>
       |    x [18:x =>...:28)
       |Term.ParamClause x [18:x:19)
       |Term.Param x [18:x:19)
       |""".stripMargin,
    """|BOF [0..0)
       |KwDef [0..3)
       |Ident(f) [4..5)
       |Equals [6..7)
       |Indentation.Indent [7..7)
       |Ident(xs) [10..12)
       |Dot [12..13)
       |Ident(map) [13..16)
       |Colon [16..17)
       |Ident(x) [18..19)
       |RightArrow [20..22)
       |Indentation.Indent [22..22)
       |Ident(x) [27..28)
       |Indentation.Outdent [33..33)
       |Indentation.Outdent [33..33)
       |EOF [33..33)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|object O:
       |  class A // c""".stripMargin,
    """|Template :
       |  class A [8<:...>19)
       |Template.Body :
       |  class A [8<:...>19)
       |Defn.Class class A [12:class A:19)
       |Type.ParamClause   class A // c@@ [24::24)
       |Ctor.Primary   class A@@ // c [19::19)
       |Template   class A // c@@ [24::24)
       |Template.Body   class A // c@@ [24::24)
       |""".stripMargin,
    """|BOF [0..0)
       |KwObject [0..6)
       |Ident(O) [7..8)
       |Colon [8..9)
       |Indentation.Indent [9..9)
       |KwClass [12..17)
       |Ident(A) [18..19)
       |Indentation.Outdent [24..24)
       |EOF [24..24)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|object O:
       |  type T // c""".stripMargin,
    """|Template :
       |  type T [8<:...>18)
       |Template.Body :
       |  type T [8<:...>18)
       |Decl.Type type T [12:type T:18)
       |Type.ParamClause   type T // c@@ [23::23)
       |Type.Bounds   type T // c@@ [23::23)
       |""".stripMargin,
    """|BOF [0..0)
       |KwObject [0..6)
       |Ident(O) [7..8)
       |Colon [8..9)
       |Indentation.Indent [9..9)
       |KwType [12..16)
       |Ident(T) [17..18)
       |Indentation.Outdent [23..23)
       |EOF [23..23)
       |""".stripMargin,
    showPosition = true,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y if xs.exists: z =>
       |    z
       |  => 1""".stripMargin,
    """|Term.Match x match
       |  case y if xs.exists: z =>
       |    z
       |  => 1
       |Term.CasesBlock case y if xs.exists: z =>
       |    z
       |  => 1
       |Case case y if xs.exists: z =>
       |    z
       |  => 1
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y if xs.exists: z =>
       |      z
       |    => 1""".stripMargin,
    """|Term.Match x match
       |  case y if xs.exists: z =>
       |      z
       |    => 1
       |Term.CasesBlock case y if xs.exists: z =>
       |      z
       |    => 1
       |Case case y if xs.exists: z =>
       |      z
       |    => 1
       |Term.Apply xs.exists: z =>
       |      z
       |Term.Select xs.exists
       |Term.Block : z =>
       |      z
       |Term.Function z =>
       |      z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y if xs.exists: z =>
       |    z
       |  && b => 1""".stripMargin,
    """|Term.Match x match
       |  case y if xs.exists: z =>
       |    z
       |  && b => 1
       |Term.CasesBlock case y if xs.exists: z =>
       |    z
       |  && b => 1
       |Case case y if xs.exists: z =>
       |    z
       |  && b => 1
       |Term.ApplyInfix xs.exists: z =>
       |    z
       |  && b
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |Type.ArgClause   && @@b => 1
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y if xs.map: z =>
       |    z + 1
       |  .contains(3) => 1""".stripMargin,
    """|Term.Match x match
       |  case y if xs.map: z =>
       |    z + 1
       |  .contains(3) => 1
       |Term.CasesBlock case y if xs.map: z =>
       |    z + 1
       |  .contains(3) => 1
       |Case case y if xs.map: z =>
       |    z + 1
       |  .contains(3) => 1
       |Term.Apply xs.map: z =>
       |    z + 1
       |  .contains(3)
       |Term.Select xs.map: z =>
       |    z + 1
       |  .contains
       |Term.Apply xs.map: z =>
       |    z + 1
       |Term.Select xs.map
       |Term.Block : z =>
       |    z + 1
       |Term.Function z =>
       |    z + 1
       |Term.ParamClause z
       |Term.Param z
       |Term.ApplyInfix z + 1
       |Type.ArgClause     z + @@1
       |Term.ArgClause (3)
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y if xs.contains:
       |    y
       |  => 1""".stripMargin,
    """|Term.Match x match
       |  case y if xs.contains:
       |    y
       |  => 1
       |Term.CasesBlock case y if xs.contains:
       |    y
       |  => 1
       |Case case y if xs.contains:
       |    y
       |  => 1
       |Term.Apply xs.contains:
       |    y
       |Term.Select xs.contains
       |Term.Block :
       |    y
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y if xs.contains:
       |    y
       |  && b => 1""".stripMargin,
    """|Term.Match x match
       |  case y if xs.contains:
       |    y
       |  && b => 1
       |Term.CasesBlock case y if xs.contains:
       |    y
       |  && b => 1
       |Case case y if xs.contains:
       |    y
       |  && b => 1
       |Term.ApplyInfix xs.contains:
       |    y
       |  && b
       |Term.Apply xs.contains:
       |    y
       |Term.Select xs.contains
       |Term.Block :
       |    y
       |Type.ArgClause   && @@b => 1
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y if xs.filter:
       |    _ > 0
       |  .nonEmpty => 1""".stripMargin,
    """|Term.Match x match
       |  case y if xs.filter:
       |    _ > 0
       |  .nonEmpty => 1
       |Term.CasesBlock case y if xs.filter:
       |    _ > 0
       |  .nonEmpty => 1
       |Case case y if xs.filter:
       |    _ > 0
       |  .nonEmpty => 1
       |Term.Select xs.filter:
       |    _ > 0
       |  .nonEmpty
       |Term.Apply xs.filter:
       |    _ > 0
       |Term.Select xs.filter
       |Term.Block :
       |    _ > 0
       |Term.AnonymousFunction _ > 0
       |Term.ApplyInfix _ > 0
       |Term.Placeholder _
       |Type.ArgClause     _ > @@0
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match {
       |  case y if xs.exists: z =>
       |    z
       |  => 1
       |}""".stripMargin,
    """|Term.Match x match {
       |  case y if xs.exists: z =>
       |    z
       |  => 1
       |}
       |Term.CasesBlock {
       |  case y if xs.exists: z =>
       |    z
       |  => 1
       |}
       |Case case y if xs.exists: z =>
       |    z
       |  => 1
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = xs.collect {
       |  case y if ys.exists: z =>
       |    z
       |  => 1
       |}""".stripMargin,
    """|Term.Apply xs.collect {
       |  case y if ys.exists: z =>
       |    z
       |  => 1
       |}
       |Term.Select xs.collect
       |Term.PartialFunction {
       |  case y if ys.exists: z =>
       |    z
       |  => 1
       |}
       |Case case y if ys.exists: z =>
       |    z
       |  => 1
       |Term.Apply ys.exists: z =>
       |    z
       |Term.Select ys.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = xs.collect:
       |  case y if ys.exists: z =>
       |    z
       |  => 1""".stripMargin,
    """|Term.Apply xs.collect:
       |  case y if ys.exists: z =>
       |    z
       |  => 1
       |Term.Select xs.collect
       |Term.PartialFunction :
       |  case y if ys.exists: z =>
       |    z
       |  => 1
       |Case case y if ys.exists: z =>
       |    z
       |  => 1
       |Term.Apply ys.exists: z =>
       |    z
       |Term.Select ys.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = try g catch
       |  case y if ys.exists: z =>
       |    z
       |  => 1""".stripMargin,
    """|Term.Try try g catch
       |  case y if ys.exists: z =>
       |    z
       |  => 1
       |Term.CasesBlock case y if ys.exists: z =>
       |    z
       |  => 1
       |Case case y if ys.exists: z =>
       |    z
       |  => 1
       |Term.Apply ys.exists: z =>
       |    z
       |Term.Select ys.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = try g catch {
       |  case y if ys.exists: z =>
       |    z
       |  => 1
       |}""".stripMargin,
    """|Term.Try try g catch {
       |  case y if ys.exists: z =>
       |    z
       |  => 1
       |}
       |Term.CasesBlock {
       |  case y if ys.exists: z =>
       |    z
       |  => 1
       |}
       |Case case y if ys.exists: z =>
       |    z
       |  => 1
       |Term.Apply ys.exists: z =>
       |    z
       |Term.Select ys.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = try g catch
       |  case y if ys.exists: z =>
       |    z
       |  => 1
       |finally h""".stripMargin,
    """|Term.Try try g catch
       |  case y if ys.exists: z =>
       |    z
       |  => 1
       |finally h
       |Term.CasesBlock case y if ys.exists: z =>
       |    z
       |  => 1
       |Case case y if ys.exists: z =>
       |    z
       |  => 1
       |Term.Apply ys.exists: z =>
       |    z
       |Term.Select ys.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = try g catch case y if ys.exists: z =>
       |  z
       |=> 1""".stripMargin,
    """|Term.Try try g catch case y if ys.exists: z =>
       |  z
       |=> 1
       |Term.CasesBlock case y if ys.exists: z =>
       |  z
       |=> 1
       |Case case y if ys.exists: z =>
       |  z
       |=> 1
       |Term.Apply ys.exists: z =>
       |  z
       |Term.Select ys.exists
       |Term.Block : z =>
       |  z
       |Term.Function z =>
       |  z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = xs.collect { case y if ys.exists: z =>
       |    z
       |  => 1
       |}""".stripMargin,
    """|Term.Apply xs.collect { case y if ys.exists: z =>
       |    z
       |  => 1
       |}
       |Term.Select xs.collect
       |Term.PartialFunction { case y if ys.exists: z =>
       |    z
       |  => 1
       |}
       |Case case y if ys.exists: z =>
       |    z
       |  => 1
       |Term.Apply ys.exists: z =>
       |    z
       |Term.Select ys.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case 0 => 0
       |  case y if xs.exists: z =>
       |    z
       |  => 1""".stripMargin,
    """|Term.Match x match
       |  case 0 => 0
       |  case y if xs.exists: z =>
       |    z
       |  => 1
       |Term.CasesBlock case 0 => 0
       |  case y if xs.exists: z =>
       |    z
       |  => 1
       |Case case 0 => 0
       |Case case y if xs.exists: z =>
       |    z
       |  => 1
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y =>
       |    y match
       |      case w if ws.exists: z =>
       |        z
       |      => 1
       |  case _ => 2""".stripMargin,
    """|Term.Match x match
       |  case y =>
       |    y match
       |      case w if ws.exists: z =>
       |        z
       |      => 1
       |  case _ => 2
       |Term.CasesBlock case y =>
       |    y match
       |      case w if ws.exists: z =>
       |        z
       |      => 1
       |  case _ => 2
       |Case case y =>
       |    y match
       |      case w if ws.exists: z =>
       |        z
       |      => 1
       |Term.Match y match
       |      case w if ws.exists: z =>
       |        z
       |      => 1
       |Term.CasesBlock case w if ws.exists: z =>
       |        z
       |      => 1
       |Case case w if ws.exists: z =>
       |        z
       |      => 1
       |Term.Apply ws.exists: z =>
       |        z
       |Term.Select ws.exists
       |Term.Block : z =>
       |        z
       |Term.Function z =>
       |        z
       |Term.ParamClause z
       |Term.Param z
       |Case case _ => 2
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f =
       |  x match
       |    case y if xs.exists: z =>
       |      z
       |    => 1
       |  g""".stripMargin,
    """|Term.Block x match
       |    case y if xs.exists: z =>
       |      z
       |    => 1
       |  g
       |Term.Match x match
       |    case y if xs.exists: z =>
       |      z
       |    => 1
       |Term.CasesBlock case y if xs.exists: z =>
       |      z
       |    => 1
       |Case case y if xs.exists: z =>
       |      z
       |    => 1
       |Term.Apply xs.exists: z =>
       |      z
       |Term.Select xs.exists
       |Term.Block : z =>
       |      z
       |Term.Function z =>
       |      z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f =
       |  x match
       |    case y if xs.exists: z =>
       |      z
       |    => 1
       |  end match""".stripMargin,
    """|Term.Block x match
       |    case y if xs.exists: z =>
       |      z
       |    => 1
       |  end match
       |Term.Match x match
       |    case y if xs.exists: z =>
       |      z
       |    => 1
       |Term.CasesBlock case y if xs.exists: z =>
       |      z
       |    => 1
       |Case case y if xs.exists: z =>
       |      z
       |    => 1
       |Term.Apply xs.exists: z =>
       |      z
       |Term.Select xs.exists
       |Term.Block : z =>
       |      z
       |Term.Function z =>
       |      z
       |Term.ParamClause z
       |Term.Param z
       |Term.EndMarker end match
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|a match
       |case n
       |if foo: b =>
       |  true
       |=>
       |  2""".stripMargin,
    """|Term.CasesBlock case n
       |if foo: b =>
       |  true
       |=>
       |  2
       |Case case n
       |if foo: b =>
       |  true
       |=>
       |  2
       |Term.Apply foo: b =>
       |  true
       |Term.Block : b =>
       |  true
       |Term.Function b =>
       |  true
       |Term.ParamClause b
       |Term.Param b
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f(x: Any) =
       |  x match
       |  case s: String if g:
       |    s.length > 4
       |  => s
       |  case x => x.toString""".stripMargin,
    """|Member.ParamClauseGroup (x: Any)
       |Type.ParamClause def f@@(x: Any) =
       |Term.ParamClause (x: Any)
       |Term.Match x match
       |  case s: String if g:
       |    s.length > 4
       |  => s
       |  case x => x.toString
       |Term.CasesBlock case s: String if g:
       |    s.length > 4
       |  => s
       |  case x => x.toString
       |Case case s: String if g:
       |    s.length > 4
       |  => s
       |Pat.Typed s: String
       |Term.Apply g:
       |    s.length > 4
       |Term.Block :
       |    s.length > 4
       |Term.ApplyInfix s.length > 4
       |Term.Select s.length
       |Type.ArgClause     s.length > @@4
       |Case case x => x.toString
       |Term.Select x.toString
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = g(x match
       |  case y if xs.exists: z =>
       |    z
       |  => 1
       |)""".stripMargin,
    """|Term.Apply g(x match
       |  case y if xs.exists: z =>
       |    z
       |  => 1
       |)
       |Term.ArgClause (x match
       |  case y if xs.exists: z =>
       |    z
       |  => 1
       |)
       |Term.Match x match
       |  case y if xs.exists: z =>
       |    z
       |  => 1
       |Term.CasesBlock case y if xs.exists: z =>
       |    z
       |  => 1
       |Case case y if xs.exists: z =>
       |    z
       |  => 1
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case s: String if xs.exists: z =>
       |    z == s
       |  => 1
       |  case _ => 2""".stripMargin,
    """|Term.Match x match
       |  case s: String if xs.exists: z =>
       |    z == s
       |  => 1
       |  case _ => 2
       |Term.CasesBlock case s: String if xs.exists: z =>
       |    z == s
       |  => 1
       |  case _ => 2
       |Case case s: String if xs.exists: z =>
       |    z == s
       |  => 1
       |Pat.Typed s: String
       |Term.Apply xs.exists: z =>
       |    z == s
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z == s
       |Term.Function z =>
       |    z == s
       |Term.ParamClause z
       |Term.Param z
       |Term.ApplyInfix z == s
       |Type.ArgClause     z == @@s
       |Case case _ => 2
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case Some(s) if xs.exists: z =>
       |    z == s
       |  => 1
       |  case _ => 2""".stripMargin,
    """|Term.Match x match
       |  case Some(s) if xs.exists: z =>
       |    z == s
       |  => 1
       |  case _ => 2
       |Term.CasesBlock case Some(s) if xs.exists: z =>
       |    z == s
       |  => 1
       |  case _ => 2
       |Case case Some(s) if xs.exists: z =>
       |    z == s
       |  => 1
       |Pat.Extract Some(s)
       |Pat.ArgClause (s)
       |Term.Apply xs.exists: z =>
       |    z == s
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z == s
       |Term.Function z =>
       |    z == s
       |Term.ParamClause z
       |Term.Param z
       |Term.ApplyInfix z == s
       |Type.ArgClause     z == @@s
       |Case case _ => 2
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case A
       |     | B if xs.exists: z =>
       |         z
       |  => 1""".stripMargin,
    """|Term.Match x match
       |  case A
       |     | B if xs.exists: z =>
       |         z
       |  => 1
       |Term.CasesBlock case A
       |     | B if xs.exists: z =>
       |         z
       |  => 1
       |Case case A
       |     | B if xs.exists: z =>
       |         z
       |  => 1
       |Pat.Alternative A
       |     | B
       |Term.Apply xs.exists: z =>
       |         z
       |Term.Select xs.exists
       |Term.Block : z =>
       |         z
       |Term.Function z =>
       |         z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case s @ Some(_) if xs.exists: z =>
       |    z
       |  => 1""".stripMargin,
    """|Term.Match x match
       |  case s @ Some(_) if xs.exists: z =>
       |    z
       |  => 1
       |Term.CasesBlock case s @ Some(_) if xs.exists: z =>
       |    z
       |  => 1
       |Case case s @ Some(_) if xs.exists: z =>
       |    z
       |  => 1
       |Pat.Bind s @ Some(_)
       |Pat.Extract Some(_)
       |Pat.ArgClause (_)
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case Some(
       |         y) if xs.exists: z =>
       |    z
       |  => 1""".stripMargin,
    """|Term.Match x match
       |  case Some(
       |         y) if xs.exists: z =>
       |    z
       |  => 1
       |Term.CasesBlock case Some(
       |         y) if xs.exists: z =>
       |    z
       |  => 1
       |Case case Some(
       |         y) if xs.exists: z =>
       |    z
       |  => 1
       |Pat.Extract Some(
       |         y)
       |Pat.ArgClause (
       |         y)
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y if
       |      xs.exists: z =>
       |        z
       |  => 1""".stripMargin,
    """|Term.Match x match
       |  case y if
       |      xs.exists: z =>
       |        z
       |  => 1
       |Term.CasesBlock case y if
       |      xs.exists: z =>
       |        z
       |  => 1
       |Case case y if
       |      xs.exists: z =>
       |        z
       |  => 1
       |Term.Apply xs.exists: z =>
       |        z
       |Term.Select xs.exists
       |Term.Block : z =>
       |        z
       |Term.Function z =>
       |        z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y
       |    if xs.exists: z =>
       |      z
       |    => 1""".stripMargin,
    """|Term.Match x match
       |  case y
       |    if xs.exists: z =>
       |      z
       |    => 1
       |Term.CasesBlock case y
       |    if xs.exists: z =>
       |      z
       |    => 1
       |Case case y
       |    if xs.exists: z =>
       |      z
       |    => 1
       |Term.Apply xs.exists: z =>
       |      z
       |Term.Select xs.exists
       |Term.Block : z =>
       |      z
       |Term.Function z =>
       |      z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y if xs.exists: z =>
       |    z => 1""".stripMargin,
    """|Term.Match x match
       |  case y if xs.exists: z =>
       |    z => 1
       |Term.CasesBlock case y if xs.exists: z =>
       |    z => 1
       |Case case y if xs.exists: z =>
       |    z => 1
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y if xs.exists: z =>
       |    z =>
       |    1""".stripMargin,
    """|Term.Match x match
       |  case y if xs.exists: z =>
       |    z =>
       |    1
       |Term.CasesBlock case y if xs.exists: z =>
       |    z =>
       |    1
       |Case case y if xs.exists: z =>
       |    z =>
       |    1
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y if xs.exists: z =>
       |    z
       |    => 1""".stripMargin,
    """|Term.Match x match
       |  case y if xs.exists: z =>
       |    z
       |    => 1
       |Term.CasesBlock case y if xs.exists: z =>
       |    z
       |    => 1
       |Case case y if xs.exists: z =>
       |    z
       |    => 1
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y if xs.contains:
       |    y => 1""".stripMargin,
    """|Term.Match x match
       |  case y if xs.contains:
       |    y => 1
       |Term.CasesBlock case y if xs.contains:
       |    y => 1
       |Case case y if xs.contains:
       |    y => 1
       |Term.Apply xs.contains:
       |    y
       |Term.Select xs.contains
       |Term.Block :
       |    y
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y if xs.contains:
       |    y =>
       |    1""".stripMargin,
    """|Term.Match x match
       |  case y if xs.contains:
       |    y =>
       |    1
       |Term.CasesBlock case y if xs.contains:
       |    y =>
       |    1
       |Case case y if xs.contains:
       |    y =>
       |    1
       |Term.Apply xs.contains:
       |    y
       |Term.Select xs.contains
       |Term.Block :
       |    y
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y if xs.contains:
       |    y
       |    => 1""".stripMargin,
    """|Term.Match x match
       |  case y if xs.contains:
       |    y
       |    => 1
       |Term.CasesBlock case y if xs.contains:
       |    y
       |    => 1
       |Case case y if xs.contains:
       |    y
       |    => 1
       |Term.Apply xs.contains:
       |    y
       |Term.Select xs.contains
       |Term.Block :
       |    y
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match {
       |  case y if xs.exists: z =>
       |    z => 1
       |}""".stripMargin,
    """|Term.Match x match {
       |  case y if xs.exists: z =>
       |    z => 1
       |}
       |Term.CasesBlock {
       |  case y if xs.exists: z =>
       |    z => 1
       |}
       |Case case y if xs.exists: z =>
       |    z => 1
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = xs.collect {
       |  case y if ys.exists: z =>
       |    z => 1
       |}""".stripMargin,
    """|Term.Apply xs.collect {
       |  case y if ys.exists: z =>
       |    z => 1
       |}
       |Term.Select xs.collect
       |Term.PartialFunction {
       |  case y if ys.exists: z =>
       |    z => 1
       |}
       |Case case y if ys.exists: z =>
       |    z => 1
       |Term.Apply ys.exists: z =>
       |    z
       |Term.Select ys.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = xs.collect:
       |  case y if ys.exists: z =>
       |    z => 1""".stripMargin,
    """|Term.Apply xs.collect:
       |  case y if ys.exists: z =>
       |    z => 1
       |Term.Select xs.collect
       |Term.PartialFunction :
       |  case y if ys.exists: z =>
       |    z => 1
       |Case case y if ys.exists: z =>
       |    z => 1
       |Term.Apply ys.exists: z =>
       |    z
       |Term.Select ys.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = try g catch
       |  case y if ys.exists: z =>
       |    z => 1""".stripMargin,
    """|Term.Try try g catch
       |  case y if ys.exists: z =>
       |    z => 1
       |Term.CasesBlock case y if ys.exists: z =>
       |    z => 1
       |Case case y if ys.exists: z =>
       |    z => 1
       |Term.Apply ys.exists: z =>
       |    z
       |Term.Select ys.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = try g catch {
       |  case y if ys.exists: z =>
       |    z => 1
       |}""".stripMargin,
    """|Term.Try try g catch {
       |  case y if ys.exists: z =>
       |    z => 1
       |}
       |Term.CasesBlock {
       |  case y if ys.exists: z =>
       |    z => 1
       |}
       |Case case y if ys.exists: z =>
       |    z => 1
       |Term.Apply ys.exists: z =>
       |    z
       |Term.Select ys.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = try g catch
       |  case y if ys.exists: z =>
       |    z => 1
       |finally h""".stripMargin,
    """|Term.Try try g catch
       |  case y if ys.exists: z =>
       |    z => 1
       |finally h
       |Term.CasesBlock case y if ys.exists: z =>
       |    z => 1
       |Case case y if ys.exists: z =>
       |    z => 1
       |Term.Apply ys.exists: z =>
       |    z
       |Term.Select ys.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = try g catch case y if ys.exists: z =>
       |  z => 1""".stripMargin,
    """|Term.Try try g catch case y if ys.exists: z =>
       |  z => 1
       |Term.CasesBlock case y if ys.exists: z =>
       |  z => 1
       |Case case y if ys.exists: z =>
       |  z => 1
       |Term.Apply ys.exists: z =>
       |  z
       |Term.Select ys.exists
       |Term.Block : z =>
       |  z
       |Term.Function z =>
       |  z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = xs.collect { case y if ys.exists: z =>
       |    z => 1
       |}""".stripMargin,
    """|Term.Apply xs.collect { case y if ys.exists: z =>
       |    z => 1
       |}
       |Term.Select xs.collect
       |Term.PartialFunction { case y if ys.exists: z =>
       |    z => 1
       |}
       |Case case y if ys.exists: z =>
       |    z => 1
       |Term.Apply ys.exists: z =>
       |    z
       |Term.Select ys.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case 0 => 0
       |  case y if xs.exists: z =>
       |    z => 1""".stripMargin,
    """|Term.Match x match
       |  case 0 => 0
       |  case y if xs.exists: z =>
       |    z => 1
       |Term.CasesBlock case 0 => 0
       |  case y if xs.exists: z =>
       |    z => 1
       |Case case 0 => 0
       |Case case y if xs.exists: z =>
       |    z => 1
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y =>
       |    y match
       |      case w if ws.exists: z =>
       |        z => 1
       |  case _ => 2""".stripMargin,
    """|Term.Match x match
       |  case y =>
       |    y match
       |      case w if ws.exists: z =>
       |        z => 1
       |  case _ => 2
       |Term.CasesBlock case y =>
       |    y match
       |      case w if ws.exists: z =>
       |        z => 1
       |  case _ => 2
       |Case case y =>
       |    y match
       |      case w if ws.exists: z =>
       |        z => 1
       |Term.Match y match
       |      case w if ws.exists: z =>
       |        z => 1
       |Term.CasesBlock case w if ws.exists: z =>
       |        z => 1
       |Case case w if ws.exists: z =>
       |        z => 1
       |Term.Apply ws.exists: z =>
       |        z
       |Term.Select ws.exists
       |Term.Block : z =>
       |        z
       |Term.Function z =>
       |        z
       |Term.ParamClause z
       |Term.Param z
       |Case case _ => 2
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f =
       |  x match
       |    case y if xs.exists: z =>
       |      z => 1
       |  g""".stripMargin,
    """|Term.Block x match
       |    case y if xs.exists: z =>
       |      z => 1
       |  g
       |Term.Match x match
       |    case y if xs.exists: z =>
       |      z => 1
       |Term.CasesBlock case y if xs.exists: z =>
       |      z => 1
       |Case case y if xs.exists: z =>
       |      z => 1
       |Term.Apply xs.exists: z =>
       |      z
       |Term.Select xs.exists
       |Term.Block : z =>
       |      z
       |Term.Function z =>
       |      z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f =
       |  x match
       |    case y if xs.exists: z =>
       |      z => 1
       |  end match""".stripMargin,
    """|Term.Block x match
       |    case y if xs.exists: z =>
       |      z => 1
       |  end match
       |Term.Match x match
       |    case y if xs.exists: z =>
       |      z => 1
       |Term.CasesBlock case y if xs.exists: z =>
       |      z => 1
       |Case case y if xs.exists: z =>
       |      z => 1
       |Term.Apply xs.exists: z =>
       |      z
       |Term.Select xs.exists
       |Term.Block : z =>
       |      z
       |Term.Function z =>
       |      z
       |Term.ParamClause z
       |Term.Param z
       |Term.EndMarker end match
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|a match
       |case n
       |if foo: b =>
       |  b => 2""".stripMargin,
    """|Term.CasesBlock case n
       |if foo: b =>
       |  b => 2
       |Case case n
       |if foo: b =>
       |  b => 2
       |Term.Apply foo: b =>
       |  b
       |Term.Block : b =>
       |  b
       |Term.Function b =>
       |  b
       |Term.ParamClause b
       |Term.Param b
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f(x: Any) =
       |  x match
       |  case s: String if g:
       |    s.length > 4 => s
       |  case x => x.toString""".stripMargin,
    """|Member.ParamClauseGroup (x: Any)
       |Type.ParamClause def f@@(x: Any) =
       |Term.ParamClause (x: Any)
       |Term.Match x match
       |  case s: String if g:
       |    s.length > 4 => s
       |  case x => x.toString
       |Term.CasesBlock case s: String if g:
       |    s.length > 4 => s
       |  case x => x.toString
       |Case case s: String if g:
       |    s.length > 4 => s
       |Pat.Typed s: String
       |Term.Apply g:
       |    s.length > 4
       |Term.Block :
       |    s.length > 4
       |Term.ApplyInfix s.length > 4
       |Term.Select s.length
       |Type.ArgClause     s.length > @@4 => s
       |Case case x => x.toString
       |Term.Select x.toString
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = g(x match
       |  case y if xs.exists: z =>
       |    z => 1
       |)""".stripMargin,
    """|Term.Apply g(x match
       |  case y if xs.exists: z =>
       |    z => 1
       |)
       |Term.ArgClause (x match
       |  case y if xs.exists: z =>
       |    z => 1
       |)
       |Term.Match x match
       |  case y if xs.exists: z =>
       |    z => 1
       |Term.CasesBlock case y if xs.exists: z =>
       |    z => 1
       |Case case y if xs.exists: z =>
       |    z => 1
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case s: String if xs.exists: z =>
       |    z == s => 1
       |  case _ => 2""".stripMargin,
    """|Term.Match x match
       |  case s: String if xs.exists: z =>
       |    z == s => 1
       |  case _ => 2
       |Term.CasesBlock case s: String if xs.exists: z =>
       |    z == s => 1
       |  case _ => 2
       |Case case s: String if xs.exists: z =>
       |    z == s => 1
       |Pat.Typed s: String
       |Term.Apply xs.exists: z =>
       |    z == s
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z == s
       |Term.Function z =>
       |    z == s
       |Term.ParamClause z
       |Term.Param z
       |Term.ApplyInfix z == s
       |Type.ArgClause     z == @@s => 1
       |Case case _ => 2
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case Some(s) if xs.exists: z =>
       |    z == s => 1
       |  case _ => 2""".stripMargin,
    """|Term.Match x match
       |  case Some(s) if xs.exists: z =>
       |    z == s => 1
       |  case _ => 2
       |Term.CasesBlock case Some(s) if xs.exists: z =>
       |    z == s => 1
       |  case _ => 2
       |Case case Some(s) if xs.exists: z =>
       |    z == s => 1
       |Pat.Extract Some(s)
       |Pat.ArgClause (s)
       |Term.Apply xs.exists: z =>
       |    z == s
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z == s
       |Term.Function z =>
       |    z == s
       |Term.ParamClause z
       |Term.Param z
       |Term.ApplyInfix z == s
       |Type.ArgClause     z == @@s => 1
       |Case case _ => 2
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case A
       |     | B if xs.exists: z =>
       |         z => 1""".stripMargin,
    """|Term.Match x match
       |  case A
       |     | B if xs.exists: z =>
       |         z => 1
       |Term.CasesBlock case A
       |     | B if xs.exists: z =>
       |         z => 1
       |Case case A
       |     | B if xs.exists: z =>
       |         z => 1
       |Pat.Alternative A
       |     | B
       |Term.Apply xs.exists: z =>
       |         z
       |Term.Select xs.exists
       |Term.Block : z =>
       |         z
       |Term.Function z =>
       |         z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case s @ Some(_) if xs.exists: z =>
       |    z => 1""".stripMargin,
    """|Term.Match x match
       |  case s @ Some(_) if xs.exists: z =>
       |    z => 1
       |Term.CasesBlock case s @ Some(_) if xs.exists: z =>
       |    z => 1
       |Case case s @ Some(_) if xs.exists: z =>
       |    z => 1
       |Pat.Bind s @ Some(_)
       |Pat.Extract Some(_)
       |Pat.ArgClause (_)
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case Some(
       |         y) if xs.exists: z =>
       |    z => 1""".stripMargin,
    """|Term.Match x match
       |  case Some(
       |         y) if xs.exists: z =>
       |    z => 1
       |Term.CasesBlock case Some(
       |         y) if xs.exists: z =>
       |    z => 1
       |Case case Some(
       |         y) if xs.exists: z =>
       |    z => 1
       |Pat.Extract Some(
       |         y)
       |Pat.ArgClause (
       |         y)
       |Term.Apply xs.exists: z =>
       |    z
       |Term.Select xs.exists
       |Term.Block : z =>
       |    z
       |Term.Function z =>
       |    z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

  checkPositions[Stat](
    """|def f = x match
       |  case y
       |    if xs.exists: z =>
       |      z => 1""".stripMargin,
    """|Term.Match x match
       |  case y
       |    if xs.exists: z =>
       |      z => 1
       |Term.CasesBlock case y
       |    if xs.exists: z =>
       |      z => 1
       |Case case y
       |    if xs.exists: z =>
       |      z => 1
       |Term.Apply xs.exists: z =>
       |      z
       |Term.Select xs.exists
       |Term.Block : z =>
       |      z
       |Term.Function z =>
       |      z
       |Term.ParamClause z
       |Term.Param z
       |""".stripMargin,
  )

}
