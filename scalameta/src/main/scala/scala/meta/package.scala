package scala

import scala.meta.macros.{Api => MacroApi}
import scala.meta.syntactic.{Api => SyntacticApi}
import scala.meta.semantic.{Api => SemanticApi}
import scala.meta.tql.{Api => TQLApi}
import scala.meta.ui.{Api => UIApi}
import scala.meta.{Quasiquotes => QuasiquoteApi}

package object meta extends MacroApi with SyntacticApi with SemanticApi with TQLApi with UIApi with QuasiquoteApi
