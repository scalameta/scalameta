package scala.meta
package taxonomic

final case class SbtProjectId(name: String, root: Path) extends ProjectId

object SbtProject {
  def apply(root: Path): Project = Project(SbtProjectId("", root))
  def apply(name: String, root: Path): Project = Project(SbtProjectId(name, root))
}