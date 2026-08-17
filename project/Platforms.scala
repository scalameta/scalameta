object Platforms {

  private val envPlatform = "SCALAMETA_PLATFORM"

  /** The build's own platform axis. */
  sealed abstract class Platform(val id: String) {
    override def toString: String = id
  }

  object JVM extends Platform("jvm")
  object JS extends Platform("js")
  object Native extends Platform("native")

  val all: Seq[Platform] = Seq(JVM, JS, Native)

  def apply(id: String): Platform = all.find(_.id == id)
    .getOrElse(throw new NoSuchElementException(s"Platform '$id' is unknown"))

  private val selected: Option[Platform] = Option(System.getenv(envPlatform)).map(_.trim.toLowerCase)
    .filter(_.nonEmpty).map(apply)

  def shouldBuildPlatform(platform: Platform): Boolean = selected.forall(_ == platform)

}
