package scala.meta
package taxonomic

import org.scalameta.adt._

@root trait CrossVersion
object CrossVersion {
  @leaf object None extends CrossVersion
  @leaf object Binary extends CrossVersion
  @leaf object Full extends CrossVersion
}

final case class IncompleteMavenId(groupId: String, artifactId: String, crossVersion: CrossVersion)

final case class MavenId(groupId: String, artifactId: String, crossVersion: CrossVersion, version: String)

trait MavenIdDsl {
  implicit class XtensionMavenDslGroupId(groupId: String){
    def %(artifactId: String) = IncompleteMavenId(groupId, artifactId, CrossVersion.None)
    def %%(artifactId: String) = IncompleteMavenId(groupId, artifactId, CrossVersion.Binary)
  }

  implicit class XtensionMavenDslIncompleteMavenId(mavenId: IncompleteMavenId){
    def %(version: String) = MavenId(mavenId.groupId, mavenId.artifactId, mavenId.crossVersion, version)
  }

  implicit class XtensionMavenDslMavenId(mavenId: MavenId) {
    def cross(crossVersion: CrossVersion) = mavenId.copy(crossVersion = crossVersion)
  }
}
