package scala.meta.tests.parsers.dotty

import scala.meta._
import scala.meta.tests.parsers.BasePositionSuite

class Scala3PositionSuite extends BasePositionSuite(dialects.Scala3) {

  checkPositions[Type]("A & B")
  checkPositions[Type]("A | B")
  checkPositions[Type](
    "[X] =>> (X, X)",
    """|Type.ParamClause [X]
       |Type.ParamClause [X@@] =>> (X, X)
       |Type.Bounds [X@@] =>> (X, X)
       |Type.Tuple (X, X)
       |""".stripMargin
  )
  checkPositions[Type](
    "[_] =>> Unit",
    """|Type.ParamClause [_]
       |Type.ParamClause [_@@] =>> Unit
       |Type.Bounds [_@@] =>> Unit
       |""".stripMargin
  )
  checkPositions[Stat]("inline def f = 1")
  checkPositions[Stat](
    "open trait a",
    """|Type.ParamClause open trait a@@
       |Ctor.Primary open trait a@@
       |Template open trait a@@
       |Template.Body open trait a@@
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
    showFieldName = true
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
  )
  checkPositions[Stat](
    "export A.{ b, c, d, _ }",
    """|Importer A.{ b, c, d, _ }
       |""".stripMargin
  )
  checkPositions[Stat](
    "export a.{given Int}",
    """|Importer a.{given Int}
       |Importee.Given given Int
       |""".stripMargin
  )
  checkPositions[Stat](
    "import Instances.{ im, given Ordering[?] }",
    """|Importer Instances.{ im, given Ordering[?] }
       |Importee.Given given Ordering[?]
       |Type.Apply Ordering[?]
       |Type.ArgClause [?]
       |Type.Wildcard ?
       |Type.Bounds import Instances.{ im, given Ordering[?@@] }
       |""".stripMargin
  )
  checkPositions[Stat](
    "import File.given",
    """|Importer File.given
       |Importee.GivenAll given
       |""".stripMargin
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
       |Type.Bounds type T = @@A match {
       |""".stripMargin
  )
  checkPositions[Stat](
    """|for case a: TP <- iter if cnd do
       |  echo""".stripMargin,
    """|Term.EnumeratorsBlock case a: TP <- iter if cnd
       |Enumerator.CaseGenerator case a: TP <- iter
       |Pat.Typed a: TP
       |Enumerator.Guard if cnd
       |""".stripMargin
  )
  checkPositions[Stat](
    "infix def a(param: Int) = param",
    """|Member.ParamClauseGroup (param: Int)
       |Type.ParamClause infix def a@@(param: Int) = param
       |Term.ParamClause (param: Int)
       |""".stripMargin
  )
  checkPositions[Stat](
    "infix type or[X, Y]",
    """|Type.ParamClause [X, Y]
       |Type.ParamClause infix type or[X@@, Y]
       |Type.Bounds infix type or[X@@, Y]
       |Type.ParamClause infix type or[X, Y@@]
       |Type.Bounds infix type or[X, Y@@]
       |Type.Bounds infix type or[X, Y]@@
       |""".stripMargin
  )
  checkPositions[Stat](
    "def fn: Unit = inline if cond then truep",
    """|Term.If inline if cond then truep
       |Lit.Unit def fn: Unit = inline if cond then truep@@
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
  )
  checkPositions[Stat](
    "val extractor: (e: Entry, f: Other) => e.Key = extractKey",
    """|Type.Function (e: Entry, f: Other) => e.Key
       |Type.FuncParamClause (e: Entry, f: Other)
       |Type.TypedParam e: Entry
       |Type.TypedParam f: Other
       |Type.Select e.Key
       |""".stripMargin
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
       |Type.Bounds type F0 = @@[T] => List[T] ?=> Option[T]
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
  )

  checkPositions[Stat](
    """|(using _: C) => ???
       |""".stripMargin,
    """|Term.ParamClause (using _: C)
       |Term.Param using _: C
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |Type.Bounds val x: (C { type U = @@T } { type T = String }) # U
       |Stat.Block { type T = String }
       |Defn.Type type T = String
       |Type.ParamClause val x: (C { type U = T } { type T @@= String }) # U
       |Type.Bounds val x: (C { type U = T } { type T = @@String }) # U
       |""".stripMargin
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
       |Type.Bounds type A = @@AnyRef with
       |""".stripMargin
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
       |Type.Bounds type A = @@AnyRef:
       |""".stripMargin
  )

  checkPositions[Type](
    """|(x: X, y: Y) => Z
       |""".stripMargin,
    """|Type.FuncParamClause (x: X, y: Y)
       |Type.TypedParam x: X
       |Type.TypedParam y: Y
       |""".stripMargin
  )

  checkPositions[Type](
    """|(X*, => Y*) => Z
       |""".stripMargin,
    """|Type.FuncParamClause (X*, => Y*)
       |Type.Repeated X*
       |Type.Repeated => Y*
       |Type.ByName => Y
       |""".stripMargin
  )

  // #3219
  checkPositions[Stat](
    "(10) + 1 toInt",
    """|Term.ApplyInfix (10) + 1
       |Type.ArgClause (10) + @@1 toInt
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
  )

  // #3689 1
  checkPositions[Term](
    """|{
       |  val a = 1 // comment
       |}
       |""".stripMargin,
    """|Defn.Val val a = 1
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
       |""".stripMargin
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
    showFieldName = true
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
    showFieldName = true
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
    showFieldName = true
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
    showFieldName = true
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
    showFieldName = true
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
    showFieldName = true
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
    showFieldName = true
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
       |Indentation.Outdent [111..111)
       |Indentation.Outdent [111..111)
       |Ident(end) [114..117)
       |KwMatch [118..123)
       |Indentation.Outdent [123..123)
       |EOF [124..124)
       |""".stripMargin,
    showFieldName = true
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
       |<ctor>Ctor.Primary   // c3@@</ctor>
       |<templ>Template   // c3@@</templ>
       |<body>Template.Body   // c3@@</body>
       |""".stripMargin,
    showFieldName = true
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
    showFieldName = true
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
       |KwVal [68..71)
       |Ident(qux) [72..75)
       |Equals [76..77)
       |Ident(quux) [78..82)
       |Indentation.Outdent [82..82)
       |EOF [83..83)
       |""".stripMargin,
    showFieldName = true
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
       |KwDef [74..77)
       |Ident(quux) [78..82)
       |Equals [83..84)
       |Ident(???) [85..88)
       |LF [88..89)
       |RightBrace [89..90)
       |EOF [91..91)
       |""".stripMargin,
    showFieldName = true
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
    showFieldName = true
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
    showFieldName = true
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
    showFieldName = true
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
    showFieldName = true
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
    showFieldName = true
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
       |Indentation.Outdent [110..110)
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
    showFieldName = true
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
       |Indentation.Outdent [110..110)
       |KwFinally [114..121)
       |Ident(qux) [122..125)
       |Indentation.Outdent [125..125)
       |Indentation.Outdent [125..125)
       |KwFinally [126..133)
       |Ident(xyz) [134..137)
       |EOF [138..138)
       |""".stripMargin,
    showFieldName = true
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
    showFieldName = true
  )

}
