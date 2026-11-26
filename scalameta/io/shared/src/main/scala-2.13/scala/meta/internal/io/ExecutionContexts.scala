package scala.meta.internal.io

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object ExecutionContexts {

  /** An executor that runs each task on the thread that calls `execute`. */
  final val parasitic: ExecutionContextExecutor = ExecutionContext.parasitic

}
