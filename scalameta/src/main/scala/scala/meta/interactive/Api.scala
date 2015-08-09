package scala.meta
package interactive

import org.scalameta.annotations._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.interactive.{Context => InteractiveContext}

private[meta] trait Api {
  @hosted def load(module: Module): Module = implicitly[InteractiveContext].load(module)
}