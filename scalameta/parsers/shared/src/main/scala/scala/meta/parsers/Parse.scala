package scala.meta
package parsers

import scala.meta.inputs._
import scala.meta.internal.parsers.ScalametaParser

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to parse into ${T}")
trait Parse[T] {
  def apply(input: Input, dialect: Dialect): Parsed[T]
  def apply(input: Input)(implicit dialect: Dialect, options: ParserOptions): Parsed[T] =
    apply(input, dialect) // fallback to the original method
}

object Parse {

  implicit val parseStat: Parse[Stat] = toParse(classOf[Stat], _.parseStat())
  implicit val parseTerm: Parse[Term] = toParse(classOf[Term], _.parseTerm())
  implicit val parseTermParam: Parse[Term.Param] = toParse(classOf[Term.Param], _.parseTermParam())
  implicit val parseType: Parse[Type] = toParse(classOf[Type], _.parseType())
  implicit val parseTypeParam: Parse[Type.Param] = toParse(classOf[Type.Param], _.parseTypeParam())
  implicit val parsePat: Parse[Pat] = toParse(classOf[Pat], _.parsePat())
  implicit val parseCase: Parse[Case] = toParse(classOf[Case], _.parseCase())
  implicit val parseCtor: Parse[Ctor] = toParse(classOf[Ctor], _.parseCtor())
  implicit val parseInit: Parse[Init] = toParse(classOf[Init], _.parseInit())
  implicit val parseSelf: Parse[Self] = toParse(classOf[Self], _.parseSelf())
  implicit val parseTemplate: Parse[Template] = toParse(classOf[Template], _.parseTemplate())
  implicit val parseMod: Parse[Mod] = toParse(classOf[Mod], _.parseMod())
  implicit val parseEnumerator: Parse[Enumerator] = toParse(classOf[Enumerator], _.parseEnumerator())
  implicit val parseImporter: Parse[Importer] = toParse(classOf[Importer], _.parseImporter())
  implicit val parseImportee: Parse[Importee] = toParse(classOf[Importee], _.parseImportee())
  implicit val parseSource: Parse[Source] = toParse(classOf[Source], _.parseSource())
  implicit val parseAmmonite: Parse[MultiSource] = toParse(classOf[MultiSource], _.parseAmmonite())

  private[meta] def toParse[T <: Tree](
      treeType: Class[_ <: Tree], // for ScannerTokens
      rule: ScalametaParser => T,
  ): Parse[T] = new Parse[T] {
    def apply(input: Input, dialect: Dialect): Parsed[T] = {
      implicit val implicitDialect: Dialect = dialect
      apply(input)
    }

    override def apply(input: Input)(implicit dialect: Dialect, options: ParserOptions): Parsed[T] =
      try Parsed.Success(ScalametaParser.parse(input, treeType, rule))
      catch { case e: InputException => Parsed.Error(e.getPos, e.getShortMessage, e) }
  }

}
