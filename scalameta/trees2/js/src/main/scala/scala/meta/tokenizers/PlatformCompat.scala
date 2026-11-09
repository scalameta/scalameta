package scala.meta.tokenizers

import scala.annotation.nowarn

object PlatformCompat {

  @nowarn("cat=unused-params") // the JVM reads it
  def loadTokenize(cl: ClassLoader): Option[Tokenize] = loadTokenize
  lazy val loadTokenize: Option[Tokenize] = Tokenize.loadScalametaTokenizer

}
