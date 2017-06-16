package scala.meta
package parsersjsfacade

import scala.scalajs.js
import js.JSConverters._
import js.annotation._

import parsers._

object ScalaParser {

  // https://stackoverflow.com/a/36573183/846273
  private[this] def mergeJSObjects(objs: js.Dynamic*): js.Dynamic = {
    val result = js.Dictionary.empty[Any]
    for (source <- objs) {
      for ((key, value) <- source.asInstanceOf[js.Dictionary[Any]])
        result(key) = value
    }
    result.asInstanceOf[js.Dynamic]
  }

  def toNode(t: Tree): js.Dynamic = {
    val base = js.Dynamic.literal(
      "type" -> t.productPrefix,
      "children" -> t.children.map(toNode).toJSArray,
      "pos" -> js.Dynamic.literal(
        "start" -> t.pos.start.offset,
        "end" -> t.pos.end.offset
      )
    )

    // FIXME: this is really just for personal gratification
    // ideally `.toMap` on Tree will take care of adding these
    // attributes
    val specific = t match {
      case t: Lit => js.Dynamic.literal(
        "value" -> t.value.toString
      )
      case t: Name => js.Dynamic.literal(
        "value" -> t.value
      )
      case _ => js.Dynamic.literal()
    }

    mergeJSObjects(base, specific)
  }

  @JSExportTopLevel("default")
  @JSExportTopLevel("parse")
  def parse(code: String): js.Dictionary[Any] = {
    code.parse[Source] match {
      case Parsed.Success(t) => toNode(t).asInstanceOf[js.Dictionary[Any]]
      case Parsed.Error(_, message, _) => js.Dictionary(
        "error" -> message
      )
    }
  }

}
