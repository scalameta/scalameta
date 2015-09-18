package scala.meta

import org.scalameta.algebra._
import scala.language.implicitConversions
import scala.meta.internal.tql._
import scala.meta.internal.{ast => impl}

// TODO: I'd love to hide this in something like `trait ExtendedTqlApi`,
// but unfortunately this cake contains class definitions,
// so we really need to instantiate it immediately.
package object tql extends Traverser[Tree]
                      with Combinators[Tree]
                      with SyntaxEnhancer[Tree]
                      with CollectionLikeUI[Tree]
                      with tql.Api {
  def traverse[A : Monoid](tree: Tree, f: Matcher[A]): MatchResult[A] = {
    TraverserBuilder.buildFromTopSymbolDelegate[Tree, A](f,
      impl.Term.Name,
      impl.Lit.Char,
      impl.Term.Apply,
      impl.Lit.Int,
      impl.Type.Name,
      impl.Term.Param,
      impl.Type.Apply,
      impl.Term.ApplyInfix
    )
  }
}
