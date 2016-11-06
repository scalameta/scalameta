package scala.meta
package semantic

import scala.meta.artifacts.Domain

private[meta] trait Api {
  implicit def domain(implicit mirror: Mirror): Domain = {
    mirror.domain
  }
}

private[meta] trait Aliases {
  type Mirror = scala.meta.semantic.Mirror
  // there's no companion for Mirror, so we don't have a term alias here
}
