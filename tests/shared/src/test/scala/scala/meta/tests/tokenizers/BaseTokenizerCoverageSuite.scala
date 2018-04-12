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

  def check[T <: Tree, R <: Tree](annotedSource: String)(implicit projection: Projection[T, R]): Unit =
    check0[T](annotedSource)(tree => projection(tree))

  def check[T <: Tree](annotedSource: String): Unit =
    check0[T](annotedSource)(identity[Tree] _)

  private def check0[T <: Tree](annotedSource: String)(project: T => Tree): Unit = {
    val startMarker = '→'
    val stopMarker = '←'

    val source = annotedSource
      .replaceAllLiterally(startMarker.toString, "")
      .replaceAllLiterally(stopMarker.toString, "")

    def assertPos(obtained: Int, expected: Int): Unit = {
      assert(
        obtained == expected,
        nl + source + nl +
          (" " * expected) + "^ expected" + nl +
          (" " * obtained) + "^ obtained"
      )
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
          if(odd) fansi.Back.Green ++ fansi.Color.LightBlue
          else fansi.Back.LightBlue ++ fansi.Color.Green
        odd = !odd
        acc.overlay(color, start, end)
    }

    test(markedSource.toString) {
      val tree = project(source.parse[Term].get.asInstanceOf[T])
      val tokens = tree.children.map(_.tokens).filter(_.nonEmpty)
      val tokensSorted = 
        tokens.map{ token =>
          val tokenStart = token.head.start
          val tokenEnd = token.last.end
          (tokenStart, tokenEnd)
        }.sorted

      tokensSorted.zip(markers).foreach {
        case ((tokenStart, tokenEnd), (markerStart, markerEnd)) => {
          assertPos(tokenStart, markerStart)
          assertPos(tokenEnd, markerEnd)
        }
      }

      assert(
        tokens.size == markers.size,
        s"incorrect number of tokens, expected: ${markers.size}, obtained: ${tokens.size}"
      )
    }
  }
}