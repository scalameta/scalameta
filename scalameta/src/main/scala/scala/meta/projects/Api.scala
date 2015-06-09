package scala.meta
package projects

import org.scalameta.adt._
import org.scalameta.annotations._
import scala.meta.projects.{Context => ProjectContext}

private[meta] trait Api {
  type Project = scala.meta.projects.Project
  val Project = scala.meta.projects.Project

  type Resource = scala.meta.projects.Resource
  val Resource = scala.meta.projects.Resource

  @hosted def project: Project = implicitly[ProjectContext].project
}
