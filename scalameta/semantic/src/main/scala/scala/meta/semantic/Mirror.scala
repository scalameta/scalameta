package scala.meta
package semantic

trait Mirror {
  def tpe(member: Member): Completed[Type]
  def desugar(tree: Tree): Completed[Tree]
}
