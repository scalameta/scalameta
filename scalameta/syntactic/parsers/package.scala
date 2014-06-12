package scala.meta
package syntactic

import org.scalameta.convert._

package object parsers {
  val keywords = Set(
    "abstract", "case", "do", "else", "finally", "for", "import", "lazy",
    "object", "override", "return", "sealed", "trait", "try", "var", "while",
    "catch", "class", "extends", "false", "forSome", "if", "match", "new",
    "package", "private", "super", "this", "true", "type", "with", "yield",
    "def", "final", "implicit", "null", "protected", "throw", "val", "_",
    ":", "=", "=>", "<-", "<:", "<%", ">:", "#", "@", "\u21D2", "\u2190"
  )

  trait Parse[T] extends Convert[Source, T]
  object Parse {
    def apply[T](f: Source => T): Parse[T] = new Parse[T] { def apply(source: Source): T = f(source) }
    implicit val parseCompUnit: Parse[Aux.CompUnit] = apply(source => new Parser(source).parseTopLevel())
    implicit val parseTerm: Parse[Term] = apply(source => new Parser(source).parseTerm())
    implicit val parseType: Parse[Type] = apply(source => new Parser(source).parseType())
    implicit val parseStats: Parse[List[Stmt.Template]] = apply(source => new Parser(source).parseStats())
    implicit val parseQ: Parse[Stmt] = apply(source => new Parser(source).parseQ())
    implicit val parseT: Parse[Param.Type] = apply(source => new Parser(source).parseT())
    implicit val parseP: Parse[Pat] = apply(source => new Parser(source).parseP())
    implicit val parseParam: Parse[Param] = apply(source => new Parser(source).parseParam())
    implicit val parseTypeParam: Parse[TypeParam] = apply(source => new Parser(source).parseTypeParam())
    implicit val parseArg: Parse[Arg] = apply(source => new Parser(source).parseArg())
    implicit val parseEnum: Parse[Enum] = apply(source => new Parser(source).parseEnum())
    implicit val parseMod: Parse[Mod] = apply(source => new Parser(source).parseMod())
    implicit val parseCase: Parse[Aux.Case] = apply(source => new Parser(source).parseCase())
    implicit val parseParent: Parse[Aux.Parent] = apply(source => new Parser(source).parseParent())
    implicit val parseTemplate: Parse[Aux.Template] = apply(source => new Parser(source).parseTemplate())
    implicit val parseSelf: Parse[Aux.Self] = apply(source => new Parser(source).parseSelf())
  }

  implicit class RichSource[T](val sourceLike: T)(implicit ev: Convert[T, Source]) {
    private val source: Source = ev(sourceLike)
    def parse[T](implicit ev: Parse[T]): T = ev(source)
  }
}
