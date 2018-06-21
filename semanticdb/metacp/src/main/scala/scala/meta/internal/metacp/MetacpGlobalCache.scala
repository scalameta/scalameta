package scala.meta.internal.metacp

import java.nio.file.AccessDeniedException
import java.nio.file.AtomicMoveNotSupportedException
import java.nio.file.FileAlreadyExistsException
import java.nio.file.FileSystemException
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.StandardCopyOption
import java.util.concurrent.ConcurrentHashMap
import java.util.function

object MetacpGlobalCache {

  private val cacheTargetLocks = new ConcurrentHashMap[Path, Object]()

  def clear(): Unit = cacheTargetLocks.clear()

  /**
    * Perform an expensive computation and cache the results on disk.
    *
    * Does a best-effort to:
    * - invoke the compute function at most once by using locks.
    * - ensure the cache file is always either non-existent or
    *   fully available by using files system atomic moves when
    *   supported by the file system.
    *
    * @param cacheTarget The path to store the cached results.
    * @param computeFunction The expensive compute function that writes its results into the
    *                        callback argument path, which points to a non-existing temporary file.
    *                        Once the compute function has completed, the temporary file is moved
    *                        to the target cache location.
    */
  def computeIfAbsent(cacheTarget: Path)(computeFunction: Path => Unit): Unit = {
    val lock = cacheTargetLocks.computeIfAbsent(cacheTarget, new function.Function[Path, Object] {
      override def apply(t: Path): AnyRef = new Object
    })
    lock.synchronized {
      if (!Files.exists(cacheTarget)) {
        val tmp = Files.createTempDirectory("metacp").resolve(cacheTarget.getFileName)
        computeFunction(tmp)
        tryAtomicMove(tmp, cacheTarget)
      }
    }
  }

  private object WindowsOnly {
    def unapply(e: Throwable): Option[Throwable] =
      if (scala.util.Properties.isWin) Some(e)
      else None
  }

  private def tryAtomicMove(source: Path, target: Path): Unit = {
    try {
      try Files.move(source, target, StandardCopyOption.ATOMIC_MOVE)
      catch {
        // If atomic moves are not supported we try with a regular move and hope for the best.
        case _: AtomicMoveNotSupportedException =>
          Files.move(source, target)
        // See https://github.com/scalameta/scalameta/issues/1499
        // AccessDeniedException is thrown on Windows when ATOMIC_MOVE is provided
        // and the target file already exists.
        case WindowsOnly(_: AccessDeniedException) =>
          ()
        // See https://github.com/scalameta/scalameta/issues/1521
        case WindowsOnly(e: FileSystemException)
            if e.getMessage.contains("being used by another process") =>
          ()
      }
    } catch {
      // If the target file already exists, do nothing. Can for example happen when racing
      // with another JVM-process doing the same processing.
      case _: FileAlreadyExistsException =>
        ()
    }
  }

}
