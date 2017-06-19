package scala.meta
package parsers

import scala.scalajs.js
import js.JSConverters._
import js.annotation._

object JSFacade {

  // https://stackoverflow.com/a/36573183/846273
  private[this] def mergeJSObjects(objs: js.Dynamic*): js.Dynamic = {
    val result = js.Dictionary.empty[Any]
    for (source <- objs) {
      for ((key, value) <- source.asInstanceOf[js.Dictionary[Any]])
        result(key) = value
    }
    result.asInstanceOf[js.Dynamic]
  }

  // https://github.com/scala-js/scala-js/issues/2170#issuecomment-176795604
  @js.native
  private[this] sealed trait JSLong extends js.Any
  implicit private[this] class LongJSOps(val x: Long) extends AnyVal {
    def toJSLong: JSLong = {
      if (x >= 0) (x & ((1L << 53) - 1)).toDouble
      else -((-x) & ((1L << 53) - 1)).toDouble
    }.asInstanceOf[JSLong]
  }

  private[this] def toNode(t: Tree): js.Dynamic = {
    val base = js.Dynamic.literal(
      "type" -> t.productPrefix,
      "children" -> t.children.map(toNode).toJSArray,
      "pos" -> js.Dynamic.literal(
        "start" -> t.pos.start.offset,
        "end" -> t.pos.end.offset
      )
    )

    def v[A](a: A): js.Dynamic =
      js.Dynamic.literal("value" -> a.asInstanceOf[js.Any])

    // FIXME(gabro)
    // This is to make the facade immediately useful by showing the trees' values
    // Ideally `.toMap` on Tree will take care of adding the '.value' attribute.
    // See https://github.com/scalameta/scalameta/issues/948
    val value = t match {
      case t: Lit.Int => v(t.value)
      case t: Lit.Double => v(t.value)
      case t: Lit.Float => v(t.value)
      case t: Lit.Byte => v(t.value)
      case t: Lit.Short => v(t.value)
      case t: Lit.Char => v(t.value.toString)
      case t: Lit.Long => v(t.value.toJSLong)
      case t: Lit.Boolean => v(t.value)
      case t: Lit.String => v(t.value)
      case t: Lit.Symbol => v(t.value.name)
      case t: Name => v(t.value)
      case _ => js.Dynamic.literal()
    }

    mergeJSObjects(base, value)
  }

  private[this] def parse[A <: Tree: Parse](code: String): js.Dictionary[Any] =
    code.parse[A] match {
      case Parsed.Success(t) => toNode(t).asInstanceOf[js.Dictionary[Any]]
      case Parsed.Error(_, message, _) => js.Dictionary(
        "error" -> message
      )
    }

  @JSExportTopLevel("default")
  @JSExportTopLevel("parseSource")
  def parseSource(code: String): js.Dictionary[Any] = parse[Source](code)

  @JSExportTopLevel("parseStat")
  def parseStat(code: String): js.Dictionary[Any] = parse[Stat](code)

}
