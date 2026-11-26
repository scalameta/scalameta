package scala.meta.internal.io

import scala.annotation.nowarn
import scala.concurrent.ExecutionContextExecutor
import scala.scalajs.concurrent.JSExecutionContext

object PlatformExecutionContexts {

  @nowarn("cat=unused-params") // JS has one thread
  def createContext(
      threads: Int,
      useQueue: Boolean = true,
      namePrefix: Option[String] = None,
  ): ExecutionContextExecutor = JSExecutionContext.queue

}
