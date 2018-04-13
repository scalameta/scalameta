package scala.meta.tests.tokenizers

import scala.meta._
import scala.meta.parsers.Parse

import org.scalatest.FunSuite

abstract class BaseTokenizerCoverageSuite extends FunSuite {
  private val nl = "\n"

  def checkNone[T <: Tree](source: String): Unit = {
    test(source) {
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

  implicit val repeated: Projection[Term.Apply, Term.Repeated] =
    new Projection[Term.Apply, Term.Repeated] {
      def apply(ap: Term.Apply): Term.Repeated = {
        val Term.Apply(_, List(r: Term.Repeated)) = ap
        r
      }
    }

  implicit val params: Projection[Decl.Def, Term.Param] = 
    new Projection[Decl.Def, Term.Param] {
      def apply(d: Decl.Def): Term.Param = {
        d.paramss.head.head
      }
    }

  implicit val importer: Projection[Import, Importer] = 
    new Projection[Import, Importer] {
      def apply(im: Import): Importer = {
        im.importers.head
      }
    }

  def check[R <: Tree, T <: Tree](annotedSource: String)(implicit projection: Projection[T, R]): Unit =
    check0[T](annotedSource)(tree => projection(tree))

  def check[T <: Tree](annotedSource: String): Unit =
    check0[T](annotedSource)(identity[Tree] _)

  private def check0[T <: Tree](annotedSource: String)(project: T => Tree): Unit = {
    val startMarker = '→'
    val stopMarker = '←'

    val source = annotedSource
      .replaceAllLiterally(startMarker.toString, "")
      .replaceAllLiterally(stopMarker.toString, "")

    val overlayColor1 = fansi.Back.Cyan ++ fansi.Color.Magenta
    val overlayColor2 = fansi.Back.Magenta ++ fansi.Color.Cyan

    def assertPos(positions: List[((Int, Int), (Int, Int))]): Unit = {
      val fSource = fansi.Str(source)
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

      if(!correct) {
        assert(
          false,
          s"""|mis-positionned children:
              |  obtained: ${tokens}
              |  expected: ${markers}
              |""".stripMargin
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
    val markedSource = markers.foldLeft(fansi.Str(source)) {
      case (acc, (start, end)) => 
        val color =
          if(odd) overlayColor1
          else overlayColor2
        odd = !odd
        acc.overlay(color, start, end)
    }

    test(markedSource.toString) {
      val tree = project(source.parse[Stat].get.asInstanceOf[T])
      val tokens = tree.children.map(_.tokens).filter(_.nonEmpty)
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