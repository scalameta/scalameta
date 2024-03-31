package scala.meta
package parsers

import scala.meta.inputs._
import scala.meta.internal.parsers.ScalametaParser

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "don't know how to parse into ${T}")
trait Parse[T] {
  def apply(input: Input, dialect: Dialect): Parsed[T]
}

object Parse {
  implicit lazy val parseStat: Parse[Stat] = toParse(_.parseStat())
  implicit lazy val parseTerm: Parse[Term] = toParse(_.parseTerm())
  implicit lazy val parseTermParam: Parse[Term.Param] = toParse(_.parseTermParam())
  implicit lazy val parseType: Parse[Type] = toParse(_.parseType())
  implicit lazy val parseTypeParam: Parse[Type.Param] = toParse(_.parseTypeParam())
  implicit lazy val parsePat: Parse[Pat] = toParse(_.parsePat())
  implicit lazy val parseCase: Parse[Case] = toParse(_.parseCase())
  implicit lazy val parseCtor: Parse[Ctor] = toParse(_.parseCtor())
  implicit lazy val parseInit: Parse[Init] = toParse(_.parseInit())
  implicit lazy val parseSelf: Parse[Self] = toParse(_.parseSelf())
  implicit lazy val parseTemplate: Parse[Template] = toParse(_.parseTemplate())
  implicit lazy val parseMod: Parse[Mod] = toParse(_.parseMod())
  implicit lazy val parseEnumerator: Parse[Enumerator] = toParse(_.parseEnumerator())
  implicit lazy val parseImporter: Parse[Importer] = toParse(_.parseImporter())
  implicit lazy val parseImportee: Parse[Importee] = toParse(_.parseImportee())
  implicit lazy val parseSource: Parse[Source] = toParse(_.parseSource())
  implicit lazy val parseAmmonite: Parse[MultiSource] = toParse(_.parseAmmonite())

  private def toParse[T](fn: ScalametaParser => T): Parse[T] = new Parse[T] {
    def apply(input: Input, dialect: Dialect): Parsed[T] =
      try {
        val parser = new ScalametaParser(input)(dialect)
        Parsed.Success(fn(parser))
      } catch {
        case details @ tokenizers.TokenizeException(pos, message) => Parsed
            .Error(pos, message, details)
        case details @ ParseException(pos, message) => Parsed.Error(pos, message, details)
      }
  }

}
