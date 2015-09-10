package scala.meta
package interactive

import org.scalameta.annotations._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.interactive.{Context => InteractiveContext}

private[meta] trait Api {
  implicit class XtensionInteractiveContext(c: InteractiveContext) {
    def load(artifact: Artifact): Artifact = c.load(artifact)

    def load(source: Source): Source = {
      val artifact = Artifact.Adhoc(List(source))
      val scala.meta.taxonomic.Artifact.Adhoc(Seq(source1), _, _) = c.load(artifact)
      source1
    }

    def load(sources: Source*): Seq[Source] = {
      val artifact = Artifact.Adhoc(sources.toList)
      val scala.meta.taxonomic.Artifact.Adhoc(sources1, _, _) = c.load(artifact)
      sources1
    }
  }
}