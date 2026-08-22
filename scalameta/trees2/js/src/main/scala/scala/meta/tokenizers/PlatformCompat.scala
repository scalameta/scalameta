package scala.meta.tokenizers

object PlatformCompat {

  def loadTokenize(cl: ClassLoader): Option[Tokenize] = loadTokenize
  lazy val loadTokenize: Option[Tokenize] = Tokenize.loadScalametaTokenizer

}
