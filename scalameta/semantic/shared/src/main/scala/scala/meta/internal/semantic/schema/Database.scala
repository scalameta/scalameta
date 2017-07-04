package scala.meta
package internal
package semantic
package schema

import scala.collection.immutable.Seq
import scala.meta.inputs.{Input => mInput}
import scala.meta.inputs.{Point => mPoint}
import scala.meta.inputs.{Position => mPosition}
import scala.meta.internal.semantic.{schema => s}
import scala.meta.internal.semantic.{vfs => v}
import scala.meta.io._
import scala.meta.{Dialect => mDialect}
import scala.meta.{semantic => m}
import scala.{Seq => _}
import java.io._
import scala.meta.internal.io.PathIO
import org.scalameta.data._

// NOTE: s.Attributes and friends are generated from semanticdb.proto.
// See scalameta/semantic/jvm/target/scala-<version>/src_managed/main/scala/meta/internal/semantic/schema.
