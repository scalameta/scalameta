package scala.meta.tokenizers

object PlatformCompat {

  def loadTokenize(cl: ClassLoader): Option[Tokenize] = None

  val loadTokenize: Option[Tokenize] = None

}
