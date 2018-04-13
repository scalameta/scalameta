package scala.meta.tests.tokenizers

import scala.meta._
import scala.meta.parsers.Parse

import org.scalatest.FunSuite

abstract class BaseTokenizerCoverageSuite extends FunSuite {
  private val nl = "\n"
  private val whiteOnBlack = fansi.Back.Black ++ fansi.Color.White

  def checkNone[T <: Tree](source: String): Unit = {
    val testName = fansi.Str(source).overlay(whiteOnBlack)
    test(testName.toString) {
      val tree = source.parse[Term].get.asInstanceOf[T]
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

  implicit val applyRepeated: Projection[Term.Apply, Term.Repeated] =
    new Projection[Term.Apply, Term.Repeated] {
      def apply(ap: Term.Apply): Term.Repeated = {
        val Term.Apply(_, List(r: Term.Repeated)) = ap
        r
      }
    }

  implicit val defParams: Projection[Decl.Def, Term.Param] = 
    new Projection[Decl.Def, Term.Param] {
      def apply(d: Decl.Def): Term.Param = {
        d.paramss.head.head
      }
    }

  implicit val importImporter: Projection[Import, Importer] = 
    new Projection[Import, Importer] {
      def apply(im: Import): Importer = {
        im.importers.head
      }
    }

  implicit val traitSelf: Projection[Defn.Trait, Self] = 
    new Projection[Defn.Trait, Self] {
      def apply(tr: Defn.Trait): Self = {
        tr.templ.self
      }
    }


  implicit val anonTemplate: Projection[Term.NewAnonymous, Template] = 
    new Projection[Term.NewAnonymous, Template] {
      def apply(anon: Term.NewAnonymous): Template = {
        anon.templ
      }
    }

  implicit val classTemplate: Projection[Defn.Class, Template] = 
    new Projection[Defn.Class, Template] {
      def apply(cls: Defn.Class): Template = {
        cls.templ
      }
    }

  implicit val classCtorSecondary: Projection[Defn.Class, Ctor.Secondary] =
    new Projection[Defn.Class, Ctor.Secondary] {
      def apply(cls: Defn.Class): Ctor.Secondary = {
        cls.templ.stats.head.asInstanceOf[Ctor.Secondary]
      }
    }

  implicit val defAnnotation: Projection[Defn.Def, Mod.Annot] =
    new Projection[Defn.Def, Mod.Annot] {
      def apply(in: Defn.Def): Mod.Annot = {
        in.mods.head.asInstanceOf[Mod.Annot]
      }
    }
  implicit val valPrivate: Projection[Defn.Val, Mod.Private] =
    new Projection[Defn.Val, Mod.Private] {
      def apply(in: Defn.Val): Mod.Private = {
        in.mods.head.asInstanceOf[Mod.Private]
      }
    }
  implicit val valProtected: Projection[Defn.Val, Mod.Protected] =
    new Projection[Defn.Val, Mod.Protected] {
      def apply(in: Defn.Val): Mod.Protected = {
        in.mods.head.asInstanceOf[Mod.Protected]
      }
    }
  implicit val valImplicit: Projection[Defn.Val, Mod.Implicit] =
    new Projection[Defn.Val, Mod.Implicit] {
      def apply(in: Defn.Val): Mod.Implicit = {
        in.mods.head.asInstanceOf[Mod.Implicit]
      }
    }
  implicit val valFinal: Projection[Defn.Val, Mod.Final] =
    new Projection[Defn.Val, Mod.Final] {
      def apply(in: Defn.Val): Mod.Final = {
        in.mods.head.asInstanceOf[Mod.Final]
      }
    }
  implicit val traitSealed: Projection[Defn.Trait, Mod.Sealed] =
    new Projection[Defn.Trait, Mod.Sealed] {
      def apply(in: Defn.Trait): Mod.Sealed = {
        in.mods.head.asInstanceOf[Mod.Sealed]
      }
    }
  implicit val defOverride: Projection[Defn.Def, Mod.Override] =
    new Projection[Defn.Def, Mod.Override] {
      def apply(in: Defn.Def): Mod.Override = {
        in.mods.head.asInstanceOf[Mod.Override]
      }
    }
  implicit val objectCase: Projection[Defn.Object, Mod.Case] =
    new Projection[Defn.Object, Mod.Case] {
      def apply(in: Defn.Object): Mod.Case = {
        in.mods.head.asInstanceOf[Mod.Case]
      }
    }
  implicit val classAbstract: Projection[Defn.Class, Mod.Abstract] =
    new Projection[Defn.Class, Mod.Abstract] {
      def apply(in: Defn.Class): Mod.Abstract = {
        in.mods.head.asInstanceOf[Mod.Abstract]
      }
    }
  implicit val classCovariant: Projection[Defn.Class, Mod.Covariant] =
    new Projection[Defn.Class, Mod.Covariant] {
      def apply(in: Defn.Class): Mod.Covariant = {
        in.tparams.head.mods.head.asInstanceOf[Mod.Covariant]
      }
    }
  implicit val classContravariant: Projection[Defn.Class, Mod.Contravariant] =
    new Projection[Defn.Class, Mod.Contravariant] {
      def apply(in: Defn.Class): Mod.Contravariant = {
        in.tparams.head.mods.head.asInstanceOf[Mod.Contravariant]
      }
    }
  implicit val valLazy: Projection[Defn.Val, Mod.Lazy] =
    new Projection[Defn.Val, Mod.Lazy] {
      def apply(in: Defn.Val): Mod.Lazy = {
        in.mods.head.asInstanceOf[Mod.Lazy]
      }
    }
  implicit val valValParam: Projection[Defn.Class, Mod.ValParam] =
    new Projection[Defn.Class, Mod.ValParam] {
      def apply(in: Defn.Class): Mod.ValParam = {
        in.ctor.paramss.head.head.mods.head.asInstanceOf[Mod.ValParam]
      }
    }
  implicit val varVarParam: Projection[Defn.Class, Mod.VarParam] =
    new Projection[Defn.Class, Mod.VarParam] {
      def apply(in: Defn.Class): Mod.VarParam = {
        in.ctor.paramss.head.head.mods.head.asInstanceOf[Mod.VarParam]
      }
    }
  implicit val defInline: Projection[Defn.Def, Mod.Inline] =
    new Projection[Defn.Def, Mod.Inline] {
      def apply(in: Defn.Def): Mod.Inline = {
        in.mods.head.asInstanceOf[Mod.Inline]
      }
    }
  
  def check[R <: Tree, T <: Tree](annotedSource: String)(implicit projection: Projection[T, R]): Unit =
    check0[T](annotedSource)(tree => projection(tree))

  def checkSelf[R <: Tree, T <: Tree](annotedSource: String)(implicit projection: Projection[T, R]): Unit =
    check0[T](annotedSource)(tree => projection(tree), checkChilds = false)

  def check[T <: Tree](annotedSource: String): Unit =
    check0[T](annotedSource)(identity[Tree] _)

  private def check0[T <: Tree](annotedSource: String)(project: T => Tree, checkChilds: Boolean = true): Unit = {
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
    val markedSource = markers.foldLeft(fansi.Str(source).overlay(whiteOnBlack)) {
      case (acc, (start, end)) => 
        val color =
          if(odd) overlayColor1
          else overlayColor2
        odd = !odd
        acc.overlay(color, start, end)
    }

    test(markedSource.toString) {
      val tree = project(source.parse[Stat].get.asInstanceOf[T])

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
}