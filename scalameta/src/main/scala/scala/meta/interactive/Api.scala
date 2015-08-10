package scala.meta
package interactive

import org.scalameta.annotations._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.interactive.{Context => InteractiveContext}

private[meta] trait Api {
  implicit class XtensionInteractiveContext(c: InteractiveContext) {
    def load(module: Module): Module = c.load(module)

    def load(source: Source): Source = {
      val module = Module.Adhoc(Seq(source))
      val scala.meta.taxonomic.Module.Adhoc(Seq(source1), _, _) = c.load(module)
      source1
    }

    def load(sources: Source*): Seq[Source] = {
      val module = Module.Adhoc(sources.toList)
      val scala.meta.taxonomic.Module.Adhoc(sources1, _, _) = c.load(module)
      sources1
    }
  }
}