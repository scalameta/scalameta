package scala.meta
package parsers

import scala.scalajs.js

import js.JSConverters._
import js.annotation._

object JSFacade {
  import inputs._
  import prettyprinters._

  // https://stackoverflow.com/a/36573183/846273
  private[this] def mergeJSObjects(objs: js.Dynamic*): js.Dynamic = {
    val result = js.Dictionary.empty[Any]
    for (source <- objs)
      for ((key, value) <- source.asInstanceOf[js.Dictionary[Any]]) result(key) = value
    result.asInstanceOf[js.Dynamic]
  }

  // https://github.com/scala-js/scala-js/issues/2170#issuecomment-176795604
  @js.native
  private[this] sealed trait JSLong extends js.Any
  private[this] implicit class LongJSOps(val x: Long) extends AnyVal {
    def toJSLong: JSLong = {
      if (x >= 0) (x & ((1L << 53) - 1)).toDouble else -((-x) & ((1L << 53) - 1)).toDouble
    }.asInstanceOf[JSLong]
  }

  private[this] def toNode(t: Any): js.Any = t match {
    case t: Tree => toNode(t)
    case tt: List[_] => tt.map(toNode).toJSArray
    case t: Option[_] => t.map(toNode).orUndefined
    case _ => ()
  }

  private[this] def toPosition(p: Position): js.Dynamic = js.Dynamic
    .literal("start" -> p.start, "end" -> p.end)

  private[this] def toNode(t: Tree): js.Dynamic = {
    val base = js.Dynamic.literal("type" -> t.productPrefix, "pos" -> toPosition(t.pos))

    val fields = js.Dictionary(t.productFields.zip(t.productIterator.toList).collect {
      case (name, value) => name -> toNode(value)
    }: _*).asInstanceOf[js.Dynamic]

    def v[A](a: A): js.Dynamic = js.Dynamic.literal("value" -> a.asInstanceOf[js.Any])

    val value = t match {
      case Lit.Char(value) => v(value.toString)
      case Lit.Long(value) => v(value.toJSLong)
      case Lit.Symbol(value) => v(value.name)
      case Lit(value) => v(value)
      case Name(value) => v(value)
      case _ => js.Dynamic.literal()
    }

    val syntax = t match {
      case _: Lit => js.Dynamic.literal("syntax" -> t.syntax)
      case _ => js.Dynamic.literal()
    }

    mergeJSObjects(base, fields, value, syntax)
  }

  private[this] type Settings = js.UndefOr[js.Dictionary[String]]
  private[this] val defaultSettings = js.undefined.asInstanceOf[Settings]

  private[this] def extractDialect(s: Settings): Either[String, Dialect] =
    s.toOption.flatMap(_.get("dialect")) match {
      case Some(dialectStr) => Dialect.standards.get(dialectStr) match {
          case Some(dialect) => Right(dialect)
          case None => Left(s"'$dialectStr' is not a valid dialect.")
        }
      case None => Right(dialects.Scala213)
    }

  private[this] def parse[A <: Tree: Parse](code: String, settings: Settings): js.Dictionary[Any] =
    extractDialect(settings) match {
      case Left(error) => js.Dictionary("error" -> error)
      case Right(dialect) => dialect(code).parse[A] match {
          case x: Parsed.Success[_] => toNode(x.tree).asInstanceOf[js.Dictionary[Any]]
          case x: Parsed.Error => js.Dictionary(
              "error" -> x.message,
              "pos" -> toPosition(x.pos),
              "lineNumber" -> x.pos.startLine,
              "columnNumber" -> x.pos.startColumn
            )
        }
    }

  @JSExportTopLevel("parseSource")
  def parseSource(code: String): js.Dictionary[Any] = parse[Source](code, defaultSettings)

  @JSExportTopLevel("parseSource")
  def parseSource(code: String, settings: Settings): js.Dictionary[Any] =
    parse[Source](code, settings)

  @JSExportTopLevel("parseStat")
  def parseStat(code: String): js.Dictionary[Any] = parse[Stat](code, defaultSettings)

  @JSExportTopLevel("parseStat")
  def parseStat(code: String, settings: Settings): js.Dictionary[Any] = parse[Stat](code, settings)

}
