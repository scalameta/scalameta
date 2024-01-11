case class ShadedDependency(
    groupID: String,
    artifactID: String,
    namespace: String,
    isPlatformSpecific: Boolean
)

object ShadedDependency {

  /* make sure that `.configureCross(crossPlatformShading)`
   * is added to the respective project in build.sbt */

  val all = Seq(
    ShadedDependency("com.lihaoyi", "geny", "geny", true),
    ShadedDependency("com.lihaoyi", "fastparse", "fastparse", true)
  )

}
