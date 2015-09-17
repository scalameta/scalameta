package scala.meta
package syntactic

private[meta] trait ScalametaParseApi {
  implicit def parseStat(implicit dialect: Dialect): Parse[Stat] = Parse(input => new ScalametaParser(input).parseStat())
  implicit def parseTerm(implicit dialect: Dialect): Parse[Term] = Parse(input => new ScalametaParser(input).parseTerm())
  implicit def parseTermArg(implicit dialect: Dialect): Parse[Term.Arg] = Parse(input => new ScalametaParser(input).parseTermArg())
  implicit def parseTermParam(implicit dialect: Dialect): Parse[Term.Param] = Parse(input => new ScalametaParser(input).parseTermParam())
  implicit def parseType(implicit dialect: Dialect): Parse[Type] = Parse(input => new ScalametaParser(input).parseType())
  implicit def parseTypeArg(implicit dialect: Dialect): Parse[Type.Arg] = Parse(input => new ScalametaParser(input).parseTypeArg())
  implicit def parseTypeParam(implicit dialect: Dialect): Parse[Type.Param] = Parse(input => new ScalametaParser(input).parseTypeParam())
  implicit def parsePat(implicit dialect: Dialect): Parse[Pat] = Parse(input => new ScalametaParser(input).parsePat())
  implicit def parsePatArg(implicit dialect: Dialect): Parse[Pat.Arg] = Parse(input => new ScalametaParser(input).parsePatArg())
  implicit def parsePatType(implicit dialect: Dialect): Parse[Pat.Type] = Parse(input => new ScalametaParser(input).parsePatType())
  implicit def parseCase(implicit dialect: Dialect): Parse[Case] = Parse(input => new ScalametaParser(input).parseCase())
  implicit def parseCtorCall(implicit dialect: Dialect): Parse[Ctor.Call] = Parse(input => new ScalametaParser(input).parseCtorCall())
  implicit def parseTemplate(implicit dialect: Dialect): Parse[Template] = Parse(input => new ScalametaParser(input).parseTemplate())
  implicit def parseMod(implicit dialect: Dialect): Parse[Mod] = Parse(input => new ScalametaParser(input).parseMod())
  implicit def parseEnumerator(implicit dialect: Dialect): Parse[Enumerator] = Parse(input => new ScalametaParser(input).parseEnumerator())
  implicit def parseImportee(implicit dialect: Dialect): Parse[Importee] = Parse(input => new ScalametaParser(input).parseImportee())
  implicit def parseSource(implicit dialect: Dialect): Parse[Source] = Parse(input => new ScalametaParser(input).parseSource())
}