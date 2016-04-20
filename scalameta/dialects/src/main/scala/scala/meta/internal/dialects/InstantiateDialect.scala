package scala.meta
package internal
package dialects

trait InstantiateDialect {
  val c: scala.reflect.macros.blackbox.Context
  import c.universe._

  def instantiateDialect(dialect: c.Tree): Dialect = {
    // We want to have a higher-order way to abstract over differences in dialects
    // and we're using implicits for that (implicits are values => values are higher-order => good).
    //
    // However, quasiquotes use macros, and macros are first-order, so we have a problem here.
    // Concretely, here we need to convert an implicit argument to a macro (the `dialect` tree)
    // into an instance of `Dialect` that we'll pass to the parser.
    //
    // TODO: For now I'll just prohibit quasiquotes for situations when `dialect` doesn't point to one of the predefined dialects.
    // A natural extension to this would be to allow any static value, not just predefined dialects.
    // Later on, we could further relax this restriction by doing parsing for a superset of all dialects and then
    // delaying validation of resulting ASTs until runtime.
    val dialects: Map[Symbol, Dialect] = {
      val packageObject = c.mirror.staticModule("scala.meta.dialects.package")
      def dialectDef(name: String) = packageObject.info.member(TermName(name))
      Dialect.all.map(d => (dialectDef(d.name), d)).toMap
    }
    dialects.getOrElse(dialect.tpe.termSymbol, {
      val suggestion = "to fix this, import something from scala.dialects, e.g. scala.meta.dialects.Scala211"
      val message = s"$dialect does not have precise enough type to be used in quasiquotes ($suggestion)"
      c.abort(c.enclosingPosition, message)
    })
  }
}
