package scala.meta
package dialects

private[meta] trait DialectApi {
  // NOTE: See a comment on Dialect for a reason why this didn't work out
  // type Dialect = scala.meta.dialects.Dialect

  type DialectException = scala.meta.dialects.DialectException
  val DialectException = scala.meta.dialects.DialectException
}