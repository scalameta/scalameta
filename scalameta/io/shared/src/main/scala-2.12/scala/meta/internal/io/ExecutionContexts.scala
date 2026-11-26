package scala.meta.internal.io

import scala.concurrent.{ExecutionContext, ExecutionContextExecutor}

object ExecutionContexts {

  /**
   * An executor that runs each task on the thread that calls `execute`, before `execute` returns.
   */
  final val parasitic: ExecutionContextExecutor = new ExecutionContextExecutor {
    override def execute(runnable: Runnable): Unit = runnable.run()
    override def reportFailure(t: Throwable): Unit = ExecutionContext.defaultReporter(t)
  }

}
