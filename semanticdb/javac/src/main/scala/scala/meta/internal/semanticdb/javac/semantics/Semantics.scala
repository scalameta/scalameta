package scala.meta.internal.semanticdb.javac.semantics

import com.sun.source.tree.CompilationUnitTree
import com.sun.source.util.Trees

trait Semantics extends Elements with TypeMirrors {
  val trees: Trees
  val compilationUnitTree: CompilationUnitTree
}
