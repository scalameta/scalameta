package scala.meta.internal.javacp

/**
  * Minimal utility to resolve generic signature type variables to fully qualified symbols.
  *
  * @param bindings Map from type variable names to their resolved symbols.
  * @param staticInnerClasses Set of ASM class names that are static.
  */
class Scope(bindings: Map[String, String], staticInnerClasses: Set[String]) {

  def isStatic(asmName: String): Boolean = {
    staticInnerClasses.contains(asmName)
  }

  /** Resolve a type variable name to a symbol */
  def resolve(name: String): String = {
    bindings.getOrElse(name, {
      // FIXME: fix https://github.com/scalameta/scalameta/issues/1365
      // There are still a handful of cases in spark-sql where resolution fails for some reason.
      name
    })
  }

  /** Returns new scope where name resolves to symbol, shadowing previous binding of name if any */
  def enter(name: String, symbol: String): Scope =
    new Scope(bindings.updated(name, symbol), staticInnerClasses)

  /** Returns new scope where asmName is registered as static */
  def enterStaticClass(asmName: String): Scope =
    new Scope(bindings, staticInnerClasses + asmName)

}

object Scope {
  val empty: Scope = new Scope(Map.empty, Set.empty)
}
