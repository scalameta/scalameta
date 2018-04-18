package scala.meta.tests.tokenizers

import scala.meta._
import scala.meta.parsers.Parse

import org.scalatest.FunSuite

abstract class BaseTokenizerCoverageSuite extends FunSuite {
  private val nl = "\n"
  private val whiteOnBlack = fansi.Back.Black ++ fansi.Color.White
  private val whiteOnGray = fansi.Back.DarkGray ++ fansi.Color.White
  val dotty = dialects.Dotty
  val maxCol = 73


  def checkNone[T <: Tree](source: String): Unit = {
    val tree = source.parse[Term].get.asInstanceOf[T]
    test(testName(source, tree).toString) {
      val tokens = tree.children
      assert(tokens.isEmpty)
    }
  }

  /* It's not always possible to write the syntax of a tree node directly
   * for example, Term.Repeated are wraped in Term.Apply: f(x: _*)
   * We can write an arbitary function to extract a child node.
   * ex: checkSome[Term.Param, Decl.Def](_.tokenEqual)("def f(x: A →=← 1): B")
   *   R: Term.Param, T: Decl.Def, apply: extract first parameter
   */
  trait Projection[R, T] {
    def apply(root: R): T
  }

  def projection[R, T](f: R => T): Projection[R, T] =
    new Projection[R, T] {
      def apply(tree: R): T = f(tree)
    }

  def mod[R <: Defn, T]: Projection[R, T] = 
    projection[R, T](
      in => {
        val mods = 
          in match {
            case r: Defn.Def => r.mods
            case r: Defn.Val => r.mods
            case r: Defn.Trait => r.mods
            case r: Defn.Object => r.mods
            case r: Defn.Class => r.mods
            case _ => ???
          }
        
        mods.head.asInstanceOf[T]
      }
    )

  def tmod[T]: Projection[Defn.Class, T] = projection[Defn.Class, T](_.tparams.head.mods.head.asInstanceOf[T])
  def ctorMod[T]: Projection[Defn.Class, T] = projection[Defn.Class, T](_.ctor.paramss.head.head.mods.head.asInstanceOf[T])

  implicit val applyRepeated: Projection[Term.Apply, Term.Repeated] =
    projection[Term.Apply, Term.Repeated] {
      case Term.Apply(_, List(r: Term.Repeated)) => r
      case _ => ???
    }

  implicit val defParams         : Projection[Decl.Def, Term.Param]          = projection[Decl.Def, Term.Param](_.paramss.head.head)
  implicit val importImporter    : Projection[Import, Importer]              = projection[Import, Importer](_.importers.head)
  implicit val traitSelf         : Projection[Defn.Trait, Self]              = projection[Defn.Trait, Self](_.templ.self)
  implicit val anonTemplate      : Projection[Term.NewAnonymous, Template]   = projection[Term.NewAnonymous, Template](_.templ)
  implicit val classTemplate     : Projection[Defn.Class, Template]          = projection[Defn.Class, Template](_.templ)
  implicit val classCtorSecondary: Projection[Defn.Class, Ctor.Secondary]    = projection[Defn.Class, Ctor.Secondary](_.templ.stats.head.asInstanceOf[Ctor.Secondary])
  implicit val defTypeParam      : Projection[Decl.Def, Type.Param]          = projection[Decl.Def, Type.Param](_.tparams.head)
  implicit val defAnnotation     : Projection[Defn.Def, Mod.Annot]           = mod[Defn.Def, Mod.Annot]
  implicit val valPrivate        : Projection[Defn.Val, Mod.Private]         = mod[Defn.Val, Mod.Private]
  implicit val valProtected      : Projection[Defn.Val, Mod.Protected]       = mod[Defn.Val, Mod.Protected]
  implicit val valImplicit       : Projection[Defn.Val, Mod.Implicit]        = mod[Defn.Val, Mod.Implicit]
  implicit val valFinal          : Projection[Defn.Val, Mod.Final]           = mod[Defn.Val, Mod.Final]
  implicit val traitSealed       : Projection[Defn.Trait, Mod.Sealed]        = mod[Defn.Trait, Mod.Sealed]
  implicit val defOverride       : Projection[Defn.Def, Mod.Override]        = mod[Defn.Def, Mod.Override]
  implicit val objectCase        : Projection[Defn.Object, Mod.Case]         = mod[Defn.Object, Mod.Case]
  implicit val classAbstract     : Projection[Defn.Class, Mod.Abstract]      = mod[Defn.Class, Mod.Abstract]
  implicit val valLazy           : Projection[Defn.Val, Mod.Lazy]            = mod[Defn.Val, Mod.Lazy]
  implicit val defInline         : Projection[Defn.Def, Mod.Inline]          = mod[Defn.Def, Mod.Inline]
  implicit val classCovariant    : Projection[Defn.Class, Mod.Covariant]     = tmod[Mod.Covariant]
  implicit val classContravariant: Projection[Defn.Class, Mod.Contravariant] = tmod[Mod.Contravariant]
  implicit val valValParam       : Projection[Defn.Class, Mod.ValParam]      = ctorMod[Mod.ValParam]
  implicit val varVarParam       : Projection[Defn.Class, Mod.VarParam]      = ctorMod[Mod.VarParam]

  def check[T <: Tree](annotedSource: String): Unit =
    check0[T](annotedSource)()

  def check[R <: Tree, T <: Tree](annotedSource: String)(implicit projection: Projection[T, R]): Unit =
    check0[T](annotedSource)(tree => projection(tree))

  def checkSelf[R <: Tree, T <: Tree](annotedSource: String, dialect: Dialect = dialects.Scala212)(implicit projection: Projection[T, R]): Unit =
    check0[T](annotedSource)(tree => projection(tree), checkChilds = false, dialect = dialect)

  def checkSource[T <: Tree](annotedSource: String): Unit =
    check0[T](annotedSource)(project = _.children.head, parser = Parse.parseSource)

  def checkType[T <: Tree](annotedSource: String): Unit =
    check0[T](annotedSource)(parser = Parse.parseType)

  def checkCase[T <: Tree](annotedSource: String): Unit =
    check0[T](annotedSource)(parser = Parse.parseCase)

  def checkPat[T <: Tree](annotedSource: String): Unit =
    check0[T](annotedSource)(parser = Parse.parsePat)

  def checkEnumerator[T <: Tree](annotedSource: String): Unit =
    check0[T](annotedSource)(parser = Parse.parseEnumerator)

  def checkCase0(annotedSource: String): Unit =
    check0[Case](annotedSource)(parser = Parse.parseCase)

  def checkType[T <: Tree](annotedSource: String, dialect: Dialect): Unit =
    check0[T](annotedSource)(parser = Parse.parseType, dialect = dialect)

  def checkType[R <: Tree, T <: Tree](annotedSource: String, dialect: Dialect = dialects.Scala212)(implicit projection: Projection[T, R]): Unit =
    check0[T](annotedSource)(tree => projection(tree), dialect = dialect)    

  private var oddTest = true

  private def check0[T <: Tree](annotedSource: String)(
                                project: T => Tree = identity[Tree] _, 
                                checkChilds: Boolean = true, 
                                parser: Parse[_ <: Tree] = Parse.parseStat,
                                dialect: Dialect = dialects.Scala212): Unit = {
    val startMarker = '→'
    val stopMarker = '←'

    val source = annotedSource
      .replaceAllLiterally(startMarker.toString, "")
      .replaceAllLiterally(stopMarker.toString, "")

    val overlayColor1 = fansi.Back.Blue
    val overlayColor2 = fansi.Back.Magenta

    def assertPos(positions: List[((Int, Int), (Int, Int))]): Unit = {
      val fSource = fansi.Str(source).overlay(fansi.Color.White)
      var odd = true

      val (tokens, markers, correct) = 
        positions.foldLeft((fSource, fSource, true)) {
          case ((tokens0, markers0, correct0), ((markerStart, markerEnd), (tokenStart, tokenEnd))) =>
            val color =
              if(odd) overlayColor1
              else overlayColor2
            odd = !odd

            val correct = correct0 && (tokenStart == markerStart && tokenEnd == markerEnd)
            val tokens = tokens0.overlay(color, tokenStart, tokenEnd)
            val markers = markers0.overlay(color, markerStart, markerEnd)

            (tokens, markers, correct)
        }

      val sep = 
        if(source.lines.size > 1) nl
        else ""

      if(!correct) {
        assert(
          false,
          s"""|mis-positionned children:
              |  obtained: ${sep}${tokens}
              |  expected: ${sep}${markers}""".stripMargin
        )  
      }
    }

    var markersOffset = 0
    var i = 0
    val markersBuilder = List.newBuilder[(Int, Int)]
    var lastStart: Option[Int] = None
    def error(msg: String, pos: Int): Unit = {
      test(annotedSource) {
        sys.error(
          msg + nl +
            annotedSource + nl +
            (" " * pos) + "^"
        )
      }
    }
    annotedSource.foreach { c =>
      if (c == startMarker) {
        if (lastStart.nonEmpty)
          error(s"Missing closing marker: '$stopMarker'", i)
        lastStart = Some(i - markersOffset)
        markersOffset += 1
      } else if (c == stopMarker) {
        lastStart match {
          case Some(start) => markersBuilder += ((start, i - markersOffset))
          case None => error("Unexpected closing marker", i)
        }
        lastStart = None
        markersOffset += 1
      }
      i += 1
    }

    val markers = markersBuilder.result()
    var odd = true

    val tree = 
      project(parser(Input.String(source), dialect).get.asInstanceOf[T])

    val markedSource = markers.foldLeft(testName(source, tree)) {
      case (acc, (start, end)) => 
        val color =
          if(odd) overlayColor1
          else overlayColor2
        odd = !odd
        acc.overlay(color, start, end)
    }

    test(markedSource.toString) {
      val toCheck = 
        if (checkChilds) tree.children
        else List(tree)
      val tokens = toCheck.map(_.tokens).filter(_.nonEmpty)
      val tokensSorted = 
        tokens.map{ token =>
          val tokenStart = token.head.start
          val tokenEnd = token.last.end
          (tokenStart, tokenEnd)
        }.sorted

      val noPosition = (0, 0)
      assertPos(tokensSorted.zipAll(markers, noPosition, noPosition))

      assert(
        tokens.size == markers.size,
        s"incorrect number of tokens, expected: ${markers.size}, obtained: ${tokens.size}"
      )
    }
  }



  private def testName(source: String, tree: Tree): fansi.Str = {
    val fullClassName = tree.getClass.toString.stripPrefix("class scala.meta.").replaceAllLiterally("$", ".")
    val lastDot = fullClassName.lastIndexOf(".")
    val className = fullClassName.slice(0, lastDot)

    val padSize = maxCol - (source + className).size

    val testColor =
      if(oddTest) whiteOnBlack
      else whiteOnGray

    oddTest = !oddTest

    fansi.Str(source + (" " * padSize) + className).overlay(testColor)
  }
}