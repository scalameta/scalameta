case class ShadedDependency(
    groupID: String,
    artifactID: String,
    namespace: String,
    isPlatformSpecific: Boolean
)

object ShadedDependency {

  val all = Seq(
    ShadedDependency("com.lihaoyi", "geny", "geny", true),
    ShadedDependency("com.lihaoyi", "fastparse", "fastparse", true),
    ShadedDependency("com.lihaoyi", "sourcecode", "sourcecode", true)
  )

}
