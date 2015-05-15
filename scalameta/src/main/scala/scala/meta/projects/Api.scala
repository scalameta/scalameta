package scala.meta
package projects

import org.scalameta.adt._
import org.scalameta.annotations._
import scala.meta.projects.{Context => ProjectContext}

private[meta] trait Api {
  @hosted def project: Project = implicitly[ProjectContext].project
}
