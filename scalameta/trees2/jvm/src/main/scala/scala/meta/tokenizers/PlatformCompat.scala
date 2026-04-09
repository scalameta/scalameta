package scala.meta.tokenizers

import java.util.ServiceLoader

object PlatformCompat {

  lazy val loadTokenize: Option[Tokenize] = {
    val factories = ServiceLoader.load(classOf[Tokenize]).iterator()
    if (factories.hasNext) Some(factories.next()) else None
  }

}
