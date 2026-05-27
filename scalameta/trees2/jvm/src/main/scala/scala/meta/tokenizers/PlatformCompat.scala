package scala.meta.tokenizers

import java.util.ServiceLoader

object PlatformCompat {

  def loadTokenize(cl: ClassLoader): Option[Tokenize] = {
    val factories = ServiceLoader.load(classOf[Tokenize], cl).iterator()
    if (factories.hasNext) Some(factories.next()) else None
  }

  lazy val loadTokenize: Option[Tokenize] = loadTokenize(getClass.getClassLoader)

}
