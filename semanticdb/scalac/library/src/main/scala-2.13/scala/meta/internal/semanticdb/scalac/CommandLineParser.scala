package scala.meta.internal.semanticdb.scalac

object CommandLineParser {
  def tokenize(line: String): List[String] = scala.sys.process.Parser.tokenize(line)
}
