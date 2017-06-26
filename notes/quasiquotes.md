Below you can find a comprehensive map between Scala's language constructs and various quasiquotes in scala.meta. If something's missing here, it's a bug that should be [submitted to our issue tracker](https://github.com/scalameta/scalameta/issues/new).

This specification describes quasiquote syntax using a markedly condensed notation. If you have troubles decyphering it, consult the "Legend" section in the end of the document.

## Literals

         | Quasiquote
---------|------------------------------
Literal  | `q"$lit"` (construction only), `q"${lit: Lit}"` (also deconstruction)

## Expressions (meta.Term)

                   | Quasiquote
-------------------|------------------
 This              | `q"this"`, `q"$qname.this"`
 Super             | `q"super"`, `q"$qname.super"`, `q"super[$qname]"`, `q"$qname.super[$qname]"`
 Name              | `q"$name"` (construction only), `q"${name: Term.Name}"` (also deconstruction)
 Selection         | `q"$expr.$name"`
 Interpolation     | Not supported yet
 Application       | `q"$expr(...$exprssnel)"`
 Type Application  | `q"$expr[..$tpesnel]"`
 Infix Application | `q"$expr $name[..$tpes] $expr"`, `q"$expr $name[..$tpes] (..$exprs)"`
 Unary Application | `q"!$expr", q"~$expr", q"-$expr", "+$expr"`
 Assign            | `q"$expr = $expr"`
 Return            | `q"return $expr"`
 Throw             | `q"throw $expr"`
 Ascribe           | `q"$expr: $tpe"`
 Annotate          | `q"$expr: ..@$annotsnel"`
 Tuple             | `q"(..$exprsnel)"`
 Block             | `q"{ ..$stats }"`
 If                | `q"if ($expr) $expr else $expr"`
 Match             | `q"$expr match { ..case $casesnel }"`
 Try               | `q"try $expr catch { ..case $cases } finally $expropt"`
 Try With Handler  | `q"try $expr catch $expr finally $expropt"`
 Function          | `q"(..$params) => $expr"`
 Partial Function  | `q"{ ..case $casesnel }"`
 While             | `q"while ($expr) $expr"`
 Do While          | `q"do $expr while($expr)"`
 For               | `q"for (..$enumeratorsnel) $expr"`
 For Yield         | `q"for (..$enumeratorsnel) yield $expr"`
 New               | `q"new { ..$stat } with ..$ctorcalls { $param => ..$stats }"`
 Placeholder       | `q"_"`
 Eta Expansion     | `q"$expr _"`
 Repeated          | `q"$expr: _*"`
 Literal           | `q"$lit"` (construction only), `q"${lit: Lit}"` (also deconstruction)

## Types (meta.Type)

                   | Quasiquote
-------------------|------------------------------
 Name              | `t"name"` (construction only), `t"${name: Type.Name}"` (also deconstruction)
 Selection         | `t"$ref.$tname"`
 Projection        | `t"$tpe#$tname"`
 Singleton         | `t"$ref.type"`
 Application       | `t"$tpe[..$tpesnel]`
 Infix Application | `t"$tpe $tname $tpe"`
 With              | `t"$tpe with $tpe"` (only for [supported dialects](/scalameta/dialects/src/main/scala/scala/meta/dialects/package.scala))
 And               | `t"$tpe & $tpe"` (only for [supported dialects](/scalameta/dialects/src/main/scala/scala/meta/dialects/package.scala))
 Or                | `t"$tpe | $tpe"` (only for [supported dialects](/scalameta/dialects/src/main/scala/scala/meta/dialects/package.scala))
 Function          | `t"(..$tpes) => $tpe"`
 Implicit Function | `t"implicit (..$tpes) => $tpe"`
 Tuple             | `t"(..$tpesnel)"`
 Refine            | `t"$tpeopt { ..$stats }"`
 Existential       | `t"$tpe forSome { ..$statsnel }"`
 Annotate          | `t"$tpe ..@$annotsnel"`
 Placeholder       | `t"_ >: $tpeopt <: $tpeopt"`
 By Name           | `t"=> $tpe"`
 Repeated          | `t"$tpe*"`
 Literal           | `t"$lit"` (construction only), `t"${lit: Lit}"` (also deconstruction)

## Patterns (meta.Pat) and Cases (meta.Case)

                   | Quasiquote
-------------------|----------------------------
 Wildcard          | `p"_"`
 Sequence Wildcard | `p"_*"`
 Var               | `p"$pname"` (construction only), `q"${name: Pat.Var.Term}"` (also deconstruction)
 Bind              | `p"$pname @ $pat"`
 Alternative       | `p"$pat | $pat"`
 Tuple             | `p"(..$patsnel)"`
 Extract           | `p"$ref[..$tpes](..$pats)"`
 Infix Extract     | `p"$pat $name (..$patsnel)"`
 Interpolation     | Not supported yet
 Typed             | `p"$pat: $ptpe"`
 Name              | `p"$name"` (construction only), `p"${name: Term.Name}"` (also deconstruction)
 Selection         | `p"$expr.$name"`
 Literal           | `p"$lit"` (construction only), `p"${lit: Lit}"` (also deconstruction)
 Case              | `p"case $pat if $expropt => $expr"`

## Type Patterns (meta.Pat.Type)

                   | Quasiquote
-------------------|------------------------------
 Wildcard          | `pt"_"`
 Var               | `pt"$ptname"` (construction only), `pt"${name: Pat.Var.Type}"` (also deconstruction)
 Selection         | `pt"$ref.$tname"`
 Projection        | `pt"$ptpe#$tname"`
 Singleton         | `pt"$ref.type"`
 Application       | `pt"$ptpe[..$ptpesnel]`
 Infix Application | `pt"$ptpe $tname $ptpe"`
 Function          | `pt"(..$ptpes) => $ptpe"`
 Tuple             | `pt"(..$ptpesnel)"`
 Compound          | `pt"..$ptpes { ..$stats }"`
 Existential       | `pt"$ptpe forSome { ..$statsnel }"`
 Annotate          | `pt"$ptpe ..@$annotsnel"`
 Placeholder       | `pt"_ >: $tpeopt <: $tpeopt"`
 Literal           | `pt"$lit"` (construction only), `pt"${lit: Lit}"` (also deconstruction)

## Statements (meta.Stat)

            | Quasiquote
------------|----------------------------
 Expression | `q"$expr"` (construction only), `q"${expr: Term}"` (also deconstruction)
 Member     | `q"$member"` (construction only), `q"${member: Member}"` (also deconstruction)
 Import     | `q"import ..$importersnel"`

## Members (meta.Member)

### Declarations

           | Quasiquote
-----------|------------------------------
 Val       | `q"..$mods val ..$pnamesnel: $tpe"`
 Var       | `q"..$mods var ..$pnamesnel: $tpe"`
 Def       | `q"..$mods def $name[..$tparams](...$paramss): $tpe"`
 Type      | `q"..$mods type $tname[..$tparams] >: $tpeopt <: $tpeopt"`

### Definitions

                | Quasiquote
----------------|------------------------------
 Val            | `q"..$mods val ..$patsnel: $tpeopt = $expr"`
 Var            | `q"..$mods var ..$patsnel: $tpeopt = $expropt"`
 Def            | `q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr"`
 Macro          | `q"..$mods def $name[..$tparams](...$paramss): $tpeopt = macro $expr"`
 Type           | `q"..$mods type $tname[..$tparams] = $tpe"`
 Class          | `q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends $template"`
 Trait          | `q"..$mods trait $tname[..$tparams] extends $template"`
 Object         | `q"..$mods object $name extends $template"`
 Package Object | `q"package object $name extends $template"`
 Package        | `q"package $ref { ..$stats }"`
 Primary Ctor   | `q"..$mods def this(...$paramss)"`
 Secondary Ctor | `q"..$mods def this(...$paramss) = $expr"`

### Value Parameters (meta.Term.Param)

                | Quasiquote
----------------|-------------------------------------------------
 Term Param     | `param"..$mods $paramname: $tpeopt = $expropt"`

### Type Parameters (meta.Type.Param)

                | Quasiquote
----------------|-------------------------------------------------
 Type Param     | `tparam"..$mods $tparamname[..$tparams] >: $tpeopt <: $tpeopt <% ..$tpes : ..$tpes"`

## Constructor References (meta.Ctor.Ref and meta.Term)

                     | Quasiquote
---------------------|------------------------------
 Name Reference      | `ctor"$ctorname"` (construction only), `ctor"${ctorname: Ctor.Name}"` (also deconstruction)
 Select Reference    | `ctor"$ref.$ctorname"`
 Project Reference   | `ctor"$tpe#$ctorname"`
 Function Reference  | `ctor"(..$tpes) => $tpe"`
 Annotated Reference | `ctor"$ctorname ..@annots"`
 Applied Reference   | `ctor"$ctorref(...$exprssnel)"`
 Tapplied Reference  | `ctor"$ctorref[..$tpesnel]"`

## Template (meta.Template)

           | Quasiquote
-----------|--------------------
 Template  | `template"{ ..$stats } with ..$ctorcalls { $param => ..$stats }"` (first `stats` is early initializers, second `stats` is regular statements in the body of the template).

## Modifiers (meta.Mod)

                  | Quasiquote
------------------|-----------------
 Annotation       | `mod"@$expr"`
 Private          | `mod"private"`, `mod"private[$qname]"`
 Protected        | `mod"protected"`, `mod"protected[$qname]"`
 Implicit         | `mod"implicit"`
 Final            | `mod"final"`
 Sealed           | `mod"sealed"`
 Override         | `mod"override"`
 Case             | `mod"case"`
 Abstract         | `mod"abstract"`
 Covariant        | `mod"+"`
 Contravariant    | `mod"-"`
 Lazy             | `mod"lazy"`
 Val Param        | `mod"valparam"`
 Var Param        | `mod"varparam"`
 Inline           | `mod"inline"`

## Enumerators (meta.Enum)

           | Quasiquote
-----------|------------------------------
 Generator | `enumerator"$pat <- $expr"`
 Value     | `enumerator"$pat = $expr"`
 Guard     | `enumerator"if $expr"`

## Importer (meta.Importer)

           | Quasiquote
-----------|---------------------------
 Importer  | `importer"$ref.{..$importeesnel}"`

## Importees (meta.Importee)

           | Quasiquote
-----------|---------------------------
 Name      | `importee"$iname"` (construction only), `importee"${iname: Name.Indeterminate}"` (also deconstruction)
 Rename    | `importee"$iname => $iname"`
 Unimport  | `importee"$iname => _"`
 Wildcard  | `importee"_"`

## Sources (meta.Source)

           | Quasiquote
-----------|---------------------------
 Source    | `source"..$stats"`

## Legend

The tables above define quasiquote syntax using a notation called *quasiquote templates*. A quasiquote is valid if it conforms to exactly one quasiquote template according to the following rules:

  1. Any trivia token (e.g. whitespace and comments) in a quasiquote template or a quasiquote is insignificant and is ignored for the purposes of conformance testing.

  1. Any non-trivia token in a quasiquote template, except for an unquote template, means that exactly that token is required in a quasiquote, with the following exceptions:

      1. Parentheses, brackets and braces around unquotes are oftentimes dropped if they wrap empty lists, e.g. `q"x + y"` conforms to `q"$expr $name[..$tpes] $expr"`.

      1. `with` is dropped if there are zero or one ctorcalls, e.g. both `q"new {}"` and `q"new C"` conform to `q"new { ..$stat } with ..$ctorcalls { $param => ..$stats }`.

      1. This list is probably incomplete. Please [submit an issue](https://github.com/scalameta/scalameta/issues/new) if you find any discrepancies.

  1. An *unquote template* (`$smth`, `..$smth` or `...$smth`) works as follows:

      1. First, we strip standard suffixes from `smth` using the "Suffixes" table (e.g. `exprssnel` means a non-empty list of lists of `expr`).

      1. Second, we figure out the expected type of `smth` using the "Shorthands" table (e.g. `expr` means `Term`, so `exprssnel` means `List[List[Term]]`).

      1. Third, we apply an appropriate number of replications to the unquote template to have it match the corresponding part of a quasiquote that's being tested for conformance:

          1. `$smth` can not be replicated.
          1. `..$smth` means an arbitrary mix of `$smth` and `..$smth` unquote templates separated according to their location (e.g. an empty string, `[$tpe]`, `[..$tpes, $tpe]` all conform to `[..$tpes]`, and the separator is a comma, as appropriate for a list of type arguments).
          1. `...$smth` means an arbitrary mix of `$smth`, `..$smth` and  `...$smth` unquote templates, separated according to their location (e.g. an empty string, `(...$exprss)`, `(..$exprs)($expr1, $expr2)()` all conform to `(...$exprss)`, and the separator are matching parentheses, as appropriate for a list of arguments).
          1. If a suffix of `smth` says that it's a non-empty list, then replication can't result in an empty list.
          1. If a quasiquote is used as a pattern, then some replications may be illegal (TODO: to be elaborated!).

      1. Finally, we match the unquotes after replication against the corresponding parts of the quasiquote under conformance test. There are three possibilities for a match: scala syntax, unquote, lifted unquote.

  1. If not specified explicitly, quasiquote templates work for both construction and deconstruction. In some cases, a template is only applicable to construction (e.g. it's impossible to pattern match a name without specifying an expected type explicitly, because patterns like in `term match { case q"$name" => }` will match any term, not limited to just term names).

### Shorthands

 Type                     | Shorthand     | Interpolator
--------------------------|---------------|--------------
 meta.Case                | `$case`       | `p`
 meta.Ctor                | `$ctor`       | `q`
 meta.Ctor.Name           | `$ctorname`   | `ctor`
 meta.Ctor.Ref            | `$ctorref`    | `ctor`
 meta.Enumerator          | `$enumerator` | `enumerator`
 meta.Member              | `$member`     | `q`
 meta.Mod                 | `$mod`        | `mod`
 meta.Mod.Annot           | `$annot`      | `mod`
 meta.Name.Indeterminate  | `$iname`      | Can't be constructed, only extracted from `importee"..."` and `mod"..."`
 meta.Name.Qualifier      | `$qname`      | `q`, `t`, anonymous names can't be constructed, only extracted from `mod"..."`
 meta.Pat                 | `$pat`        | `p`
 meta.Pat.Var.Term        | `$pname`      | `p`
 meta.Pat.Var.Type        | `$ptname`     | `pt`
 meta.Pat.Type            | `$ptpe`       | `pt`
 meta.Importee            | `$importee`   | `importee`
 meta.Importer            | `$importer`   | `importer`
 meta.Stat                | `$stat`       | `q`
 meta.Template            | `$template`   | `template`
 meta.Term                | `$expr`       | `q`
 meta.Term.Name           | `$name`       | `q`
 meta.Term.Ref            | `$ref`        | `q`
 meta.Term.Param          | `$param`      | `param`
 meta.Term.Param.Name     | `$paramname`  | `q`, anonymous names can't be constructed, only extracted from `param`
 meta.Type                | `$tpe`        | `t`
 meta.Type.Name           | `$tname`      | `t`
 meta.Type.Param          | `$tparam`     | `tparam`
 meta.Type.Param.Name     | `$tparamname` | `t`, anonymous names can't be constructed, only extracted from `tparam`
                          | `$lit`        | `q`

### Suffixes

 Suffix | Wrapped Type  | Example
--------|---------------|-----------------------------
 -s     | `List[_]`      | `exprs: List[meta.Term]`
 -ss    | `List[List[_]]` | `exprss: List[List[meta.Term]]`
 -opt   | `Option[_]`   | `expropt: Option[meta.Term]`
 -nel   | `_`           | `tpesnel: List[meta.Type]`
