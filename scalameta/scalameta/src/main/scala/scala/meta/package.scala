package scala

import scala.meta.dialects.DialectApi
import scala.meta.ExceptionApi
import scala.meta.interactive.InteractiveApi
import scala.meta.parsers.ScalametaParseApi
import scala.meta.ui.PrettyprintApi
import scala.meta.quasiquotes.QuasiquoteApi
import scala.meta.semantic.SemanticApi
import scala.meta.taxonomic.TaxonomicApi
import scala.meta.syntactic.ScalametaTokenizeApi
import scala.meta.tokenquasiquotes.TokenQuasiquoteApi
import scala.meta.syntactic.TokenApi
import scala.meta.syntactic.GenericTokenizeApi
import scala.meta.tql.BaseTqlApi
import scala.meta.tql.ExtendedTqlApi
import scala.meta.syntactic.SyntacticApi
import scala.meta.syntactic.GenericParseApi

package object meta extends DialectApi
                       with ExceptionApi
                       with InteractiveApi
                       with ScalametaParseApi
                       with PrettyprintApi
                       with QuasiquoteApi
                       with SemanticApi
                       with TaxonomicApi
                       with ScalametaTokenizeApi
                       with TokenQuasiquoteApi
                       with TokenApi
                       with GenericTokenizeApi
                       with BaseTqlApi
                       with SyntacticApi
                       with GenericParseApi

package meta {
  package object tql extends ExtendedTqlApi
}