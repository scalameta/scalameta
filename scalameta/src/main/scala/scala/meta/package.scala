package scala

import scala.meta.interactive.{Api => InteractiveApi}
import scala.meta.semantic.{Api => SemanticApi}
import scala.meta.syntactic.{Api => SyntacticApi}
import scala.meta.taxonomic.{Api => TaxonomicApi}
import scala.meta.tql.{Api => TQLApi}
import scala.meta.ui.{Api => UIApi}
import scala.meta.{Quasiquotes => QuasiquoteApi}

package object meta extends InteractiveApi
                       with SemanticApi
                       with SyntacticApi
                       with TaxonomicApi
                       with TQLApi
                       with UIApi
                       with QuasiquoteApi
