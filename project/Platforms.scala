object Platforms {

  private val envPlatform = "SCALAMETA_PLATFORM"

  private val platforms: Map[String, sbtcrossproject.Platform] = Map(
    "jvm" -> sbtcrossproject.JVMPlatform,
    "js" -> scalajscrossproject.JSPlatform,
    "native" -> scalanativecrossproject.NativePlatform
  )

  private val platformOpt = Option(System.getenv(envPlatform)).map(_.trim.toLowerCase)
    .filter(_.nonEmpty).map { x =>
      platforms.get(x).getOrElse(throw new NoSuchElementException(s"Platform '$x' is unknown'"))
    }

  def shouldBuildPlatform(platform: sbtcrossproject.Platform): Boolean = platformOpt.isEmpty ||
    platformOpt.contains(platform)

}
