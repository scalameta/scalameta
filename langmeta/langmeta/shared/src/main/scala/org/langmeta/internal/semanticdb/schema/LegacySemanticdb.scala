package org.langmeta.internal.semanticdb.schema

import org.langmeta.inputs

import org.{langmeta => d}
import scala.meta.internal.{semanticdb3 => s}
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}

object LegacySemanticdb {

  def toTextDocuments(database: Database): s.TextDocuments = {
    val documents = database.documents.map(toTextDocument)
    s.TextDocuments(documents)
  }

  def toTextDocument(document: Document): s.TextDocument = {

    val input = inputs.Input.VirtualFile(document.filename, document.contents)

    def toRange(pos: Position): s.Range = {
      val r = inputs.Position.Range(input, pos.start, pos.end)
      s.Range(r.startLine, r.startColumn, r.endLine, r.endColumn)
    }

    def toSymbol(symbol: String) = {
      if (symbol.startsWith("_root_")) symbol
      else if (symbol.startsWith("_empty_")) symbol
      else if (symbol.startsWith("_star_")) symbol
      // Convert old-style local symbol to new-style local symbol.
      else "local" + symbol.replace('/', '_').replace('.', '_').replace('@', '_')
    }

    val occurences = document.names.map { name =>
      s.SymbolOccurrence(
        range = name.position.map(toRange),
        symbol = toSymbol(name.symbol),
        role =
          if (name.isDefinition) s.SymbolOccurrence.Role.DEFINITION
          else s.SymbolOccurrence.Role.REFERENCE
      )
    }

    val symbols = document.symbols.map { symbol =>
      val denot = symbol.denotation.get
      val signature = toTextDocument(
        Document(
          contents = denot.signature,
          names = denot.names
        )
      )
      s.SymbolInformation(
        toSymbol(symbol.symbol),
        language = document.language,
        kind = toKind(denot.flags),
        properties = toProperty(denot.flags),
        name = denot.name,
        signature = Some(signature),
        members = denot.members,
        overrides = denot.overrides
      )
    }

    val diagnostics = document.messages.map { message =>
      s.Diagnostic(
        range = message.position.map(toRange),
        severity = message.severity match {
          case Message.Severity.INFO => s.Diagnostic.Severity.INFORMATION
          case Message.Severity.WARNING => s.Diagnostic.Severity.WARNING
          case Message.Severity.ERROR => s.Diagnostic.Severity.ERROR
          case _ => s.Diagnostic.Severity.UNKNOWN_SEVERITY
        },
        message = message.text
      )
    }

    val synthetics = document.synthetics.map { synthetic =>
      val text = toTextDocument(
        Document(
          contents = synthetic.text,
          names = synthetic.names
        ))
      s.Synthetic(
        range = synthetic.pos.map(toRange),
        text = Some(text)
      )
    }

    s.TextDocument(
      schema = s.Schema.SEMANTICDB3,
      uri = document.filename,
      text = document.contents,
      language = document.language,
      symbols = symbols,
      occurrences = occurences,
      diagnostics = diagnostics,
      synthetics = synthetics
    )
  }

  def toKind(flags: Long): s.SymbolInformation.Kind = {
    def dtest(bit: Long): Boolean = (flags & bit) == bit
    if (dtest(d.VAL)) k.VAL
    else if (dtest(d.VAR)) k.VAR
    else if (dtest(d.DEF)) k.DEF
    else if (dtest(d.PRIMARYCTOR)) k.PRIMARY_CONSTRUCTOR
    else if (dtest(d.SECONDARYCTOR)) k.SECONDARY_CONSTRUCTOR
    else if (dtest(d.MACRO)) k.MACRO
    else if (dtest(d.TYPE)) k.TYPE
    else if (dtest(d.PARAM)) k.PARAMETER
    else if (dtest(d.TYPEPARAM)) k.TYPE_PARAMETER
    else if (dtest(d.OBJECT)) k.OBJECT
    else if (dtest(d.PACKAGE)) k.PACKAGE
    else if (dtest(d.PACKAGEOBJECT)) k.PACKAGE_OBJECT
    else if (dtest(d.CLASS)) k.CLASS
    else if (dtest(d.TRAIT)) k.TRAIT
    else k.UNKNOWN_KIND
  }

  def toProperty(flags: Long): Int = {
    def dtest(bit: Long) = (flags & bit) == bit
    var sproperties = 0
    def sflip(sbit: Int): Unit = sproperties ^= sbit
    if (dtest(d.PRIVATE)) sflip(p.PRIVATE.value)
    if (dtest(d.PROTECTED)) sflip(p.PROTECTED.value)
    if (dtest(d.ABSTRACT)) sflip(p.ABSTRACT.value)
    if (dtest(d.FINAL)) sflip(p.FINAL.value)
    if (dtest(d.SEALED)) sflip(p.SEALED.value)
    if (dtest(d.IMPLICIT)) sflip(p.IMPLICIT.value)
    if (dtest(d.LAZY)) sflip(p.LAZY.value)
    if (dtest(d.CASE)) sflip(p.CASE.value)
    if (dtest(d.COVARIANT)) sflip(p.COVARIANT.value)
    if (dtest(d.CONTRAVARIANT)) sflip(p.CONTRAVARIANT.value)
    sproperties
  }

}
