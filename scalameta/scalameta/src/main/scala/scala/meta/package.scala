package scala

import scala.meta.dialects.DialectApi
import scala.meta.exceptions.ExceptionApi
import scala.meta.interactive.InteractiveApi
import scala.meta.syntactic.ScalametaParseApi
import scala.meta.ui.PrettyprintApi
import scala.meta.quasiquotes.QuasiquoteApi
import scala.meta.semantic.SemanticApi
import scala.meta.taxonomic.TaxonomicApi
import scala.meta.syntactic.ScalametaTokenizeApi
import scala.meta.tokenquasiquotes.TokenQuasiquoteApi
import scala.meta.syntactic.BasicTokenApi
import scala.meta.syntactic.GenericTokenizeApi
import scala.meta.tql.BasicTqlApi
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
                       with BasicTokenApi
                       with GenericTokenizeApi
                       with BasicTqlApi
                       with SyntacticApi
                       with GenericParseApi
