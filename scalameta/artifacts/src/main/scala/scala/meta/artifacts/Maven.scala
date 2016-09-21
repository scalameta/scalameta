package scala.meta
package artifacts

import org.scalameta.adt._
import org.scalameta.data._

@root trait CrossVersion
object CrossVersion {
  @leaf object none extends CrossVersion
  @leaf object binary extends CrossVersion
  @leaf object full extends CrossVersion
}

@data class IncompleteMavenId(groupId: String, artifactId: String, crossVersion: CrossVersion) {
  override def toString = crossVersion match {
    case CrossVersion.none => "\"" + groupId + "\" % \"" + artifactId + "\""
    case CrossVersion.binary => "\"" + groupId + "\" %% \"" + artifactId + "\""
    case CrossVersion.full => s"IncompleteMavenId($groupId, $artifactId, $crossVersion)"
  }
}

@data class MavenId(groupId: String, artifactId: String, crossVersion: CrossVersion, version: String) {
  override def toString = crossVersion match {
    case CrossVersion.none => "\"" + groupId + "\" % \"" + artifactId + "\" % \"" + version + "\""
    case CrossVersion.binary => "\"" + groupId + "\" %% \"" + artifactId + "\" % \"" + version + "\""
    case CrossVersion.full => "\"" + groupId + "\" % \"" + artifactId + "\" % \"" + version + "\" cross CrossVersion.full"
  }
}

private[meta] trait MavenDsl {
  implicit class XtensionMavenDslGroupId(groupId: String){
    def %(artifactId: String) = IncompleteMavenId(groupId, artifactId, CrossVersion.none)
    def %%(artifactId: String) = IncompleteMavenId(groupId, artifactId, CrossVersion.binary)
  }

  implicit class XtensionMavenDslIncompleteMavenId(mavenId: IncompleteMavenId){
    def %(version: String) = MavenId(mavenId.groupId, mavenId.artifactId, mavenId.crossVersion, version)
  }

  implicit class XtensionMavenDslMavenId(mavenId: MavenId) {
    def cross(crossVersion: CrossVersion) = mavenId.copy(crossVersion = crossVersion)
  }

  implicit class XtensionMavenDslMavenArtifact(mavenArtifact: Artifact.Maven) {
    def cross(crossVersion: CrossVersion) = mavenArtifact.copy(id = mavenArtifact.id.copy(crossVersion = crossVersion))
  }
}
