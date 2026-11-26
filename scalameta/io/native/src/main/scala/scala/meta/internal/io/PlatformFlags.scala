package scala.meta.internal.io

object PlatformFlags {
  final val isJS = false
  final val isJVM = false
  final val isNative = true
  val isNativeOnWindows: Boolean = scala.scalanative.runtime.Platform.isWindows()
}
