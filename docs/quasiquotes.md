Below you can find a comprehensive map between Scala's language constructs and various quasiquotes in scala.meta. If something's missing here, it's a bug that should be [submitted to our issue tracker](https://github.com/scalameta/scalameta/issues/new).

This specification describes quasiquote syntax using a markedly condensed notation. If you have troubles decyphering it, consult the "Legend" section in the end of the document.

## Literals

         | Quasiquote
---------|------------------------------
 Boolean | `q"true"`, `q"false"`, `q"$bool"` (construction only), `q"${bool: Boolean}"` (also deconstruction)
 Byte    | `q"$byte"` (construction only), `q"${byte: Byte}"` (also deconstruction)
 Short   | `q"$short"` (construction only), `q"${short: Short}"` (also deconstruction)
 Int     | `q"1"`, `q"$int"` (construction only), `q"${int: Int}"` (also deconstruction)
 Long    | `q"1L"`, `q"$long"` (construction only), `q"${long: Long}"` (also deconstruction)
 Float   | `q"1.0f"`, `q"$float"` (construction only), `q"${float: Float}"` (also deconstruction)
 Double  | `q"1.0"`, `q"1.0d"`, `q"$double"` (construction only), `q"${double: Double}"` (also deconstruction)
 Char    | `q" 'c' "`, `q"$char"` (construction only), `q"${char: Char}"` (also deconstruction)
 String  | `q""" "s" """`, `q"$string"` (construction only), `q"${string: String}"` (also deconstruction)
 Symbol  | `q" 's "`, `q"$symbol"` (construction only), `q"${symbol: Symbol}"` (also deconstruction)
 Null    | `q"null"`
 Unit    | `q"()"`

## Expressions (meta.Term)

                   | Quasiquote
-------------------|------------------
 This              | `q"this"`, `q"$qname.this"`
 Super             | `q"super"`, `q"$qname.super"`, `q"super[$qname]"`, `q"$qname.super[$qname]"`
 Name              | `q"$name"` (construction only), `q"${name: Term.Name}"` (also deconstruction)
 Selection         | `q"$expr.$name"`
 Interpolation     | Not supported yet [(#251)](https://github.com/scalameta/scalameta/issues/251)
 Application       | `q"$expr(...$aexprssnel)"`
 Type Application  | `q"$expr[..$tpesnel]"`
 Infix Application | `q"$expr $name[..$tpes] $expr"`, `q"$expr $name[..$tpes] (..$aexprsnel)"`
 Unary Application | `q"!$expr", q"~$expr", q"-$expr", "+$expr"`
 Assign            | `q"$ref = $expr"`
 Update            | `q"$expr(...$aexprssnel) = $expr"`
 Return            | `q"return $expr"`
 Throw             | `q"throw $expr"`
 Ascribe           | `q"$expr: $tpe"`
 Annotate          | `q"$expr: ..@$annotsnel"`
 Tuple             | `q"(..$exprsnel)"`
 Block             | `q"{ ..$stats }"`
 If                | `q"if ($expr) $expr else $expr"`
 Match             | `q"$expr match { ..case $casesnel }"`
 Try Catch Cases   | `q"try $expr catch { ..case $cases } finally $expropt"`
 Try Catch Expr    | `q"try $expr catch $expr finally $expropt"`
 Function          | `q"(..$params) => $expr"`
 Partial Function  | `q"{ ..case $casesnel }"`
 While             | `q"while ($expr) $expr"`
 Do While          | `q"do $expr while($expr)"`
 For               | `q"for (..$enumeratorsnel) $expr"`
 For Yield         | `q"for (..$enumeratorsnel) yield $expr"`
 New               | `q"new { ..$stat } with ..$ctorcalls { $param => ..$stats }`
 Placeholder       | `q"_"`
 Eta Expansion     | `q"$expr _"`
 Literal           | `q"$lit"` (construction only), `q"${lit: Lit}"` (also deconstruction)

## Arguments (meta.Term.Arg)

            | Quasiquote
------------|------------------------------
 Named      | `arg"$name = $expr"`
 Repeated   | `arg"$expr: _*"`
 Expression | `arg"$expr"` (construction only), `arg"${expr: Term}"` (also deconstruction)

## Types (meta.Type)

                   | Quasiquote
-------------------|------------------------------
 Name              | `t"name"` (construction only), `t"${name: Type.Name}"` (also deconstruction)
 Selection         | `t"$ref.$tname"`
 Projection        | `t"$tpe#$tname"`
 Singleton         | `t"$ref.type"`
 Application       | `t"$tpe[..$tpesnel]`
 Infix Application | `t"$tpe $tname $tpe"`
 Function          | `t"(..$atpes) => $tpe"`
 Tuple             | `t"(..$tpesnel)"`
 Compound          | `t"..$tpes { ..$stats }"`
 Existential       | `t"$tpe forSome { ..$statsnel }"`
 Annotate          | `t"$tpe ..@$annotsnel"`
 Placeholder       | `t"_ >: $tpeopt <: $tpeopt"`
 Lambda            | `t"[..$tparamsnel]$tpe"`
 Method            | `t"(...$paramss): $tpe"`
 Literal           | `t"$lit"` (construction only), `t"${lit: Lit}"` (also deconstruction)

## Argument Types (meta.Type.Arg)

          | Quasiquote
----------|-----------------
 By Name  | `targ"=> $tpe"`
 Repeated | `targ"$tpe*"`
 Type     | `targ"$tpe"` (construction only), `targ"${tpe: Type}"` (also deconstruction)

## Patterns (meta.Pat) and Cases (meta.Case)

               | Quasiquote
---------------|----------------------------
 Wildcard      | `p"_"`
 Var           | `p"$pname"` (construction only), `q"${name: Pat.Var.Term}"` (also deconstruction)
 Bind          | `p"$pname @ $apat"`
 Alternative   | `p"$pat | $pat"`
 Tuple         | `p"(..$patsnel)"`
 Extract       | `p"$ref[..$tpes](..$apats)"`
 Infix Extract | `p"$pat $name (..$apatsnel)"`
 Interpolation | Not supported yet [(#251)](https://github.com/scalameta/scalameta/issues/251)
 Typed         | `p"$pat: $ptpe"`
 Name          | `p"$name"` (construction only), `p"${name: Term.Name}"` (also deconstruction)
 Selection     | `p"$expr.$name"`
 Literal       | `p"$lit"` (construction only), `p"${lit: Lit}"` (also deconstruction)
 Case          | `p"case $pat if $expropt => $expr"`

## Argument Patterns (meta.Pat.Arg)

                   | Quasiquote
-------------------|----------------------------
 Sequence Wildcard | `parg"_*"`
 Pattern           | `parg"$pat"` (construction only), `parg"${pat: Pat}"` (also deconstruction)

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
 Import     | `q"import ..($ref.{..$importeesnel})nel"`

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
 Macro          | `q"..$mods def $name[..$tparams](...$paramss): $tpe = macro $expr"`
 Type           | `q"..$mods type $tname[..$tparams] = $tpe"`
 Class          | `q"..$mods class $tname[..$tparams] $mod (...$paramss) extends $template"`
 Trait          | `q"..$mods trait $tname[..$tparams] extends $template"`
 Object         | `q"..$mods object $name extends $template"`
 Package Object | `q"package object $name extends $template"`
 Package        | `q"package $ref { ..$stats }"`
 Primary Ctor   | `q"..$mods def this(...$paramss)"`
 Secondary Ctor | `q"..$mods def this(...$paramss) = $expr"`

### Value Parameters (meta.Term.Param)

                | Quasiquote
----------------|-------------------------------------------------
 Term Param     | `param"..$mods $paramname: $atpeopt = $expropt"`

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
 Applied Reference   | `ctor"$ctorref(...$aexprssnel)"`
 Tapplied Reference  | `ctor"$ctorref[..$atpesnel]"`

## Template (meta.Template)

           | Quasiquote
-----------|--------------------
 Template  | `template"{ ..$stats } with ..$ctorcalls { $param => ..$stats }"`

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
 Val              | `mod"val"`
 Var              | `mod"var"`

## Enumerators (meta.Enum)

           | Quasiquote
-----------|------------------------------
 Generator | `enumerator"$pat <- $expr"`
 Value     | `enumerator"$pat = $expr"`
 Guard     | `enumerator"if $expr"`

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
    1. Parentheses, brackets and braces around unquotes are oftentimes dropped if they wrap empty sequences, e.g. `q"x + y"` conforms to `q"$expr $name[..$tpes] $expr"`.

    1. `with` is dropped if there are zero or one ctorcalls, e.g. both `q"new {}"` and `q"new C"` conform to `q"new { ..$stat } with ..$ctorcalls { $param => ..$stats }`.

    1. This list is probably incomplete. Please [submit an issue](https://github.com/scalameta/scalameta/issues/new) if you find any discrepancies.

  1. An *unquote template* (`$smth`, `..$smth` or `...$smth`) works as follows:
    1. First, we strip standard suffixes from `smth` using the "Suffixes" table (e.g. `aexprssnel` means a non-empty sequence of sequences of `aexpr`).

    1. Second, we figure out the expected type of `smth` using the "Shorthands" table (e.g. `aexpr` means `Term.Arg`, so `aexprssnel` means `Seq[Seq[Term.Arg]]`).

    1. Third, we apply an appropriate number of replications to the unquote template to have it match the corresponding part of a quasiquote that's being tested for conformance:
      1. `$smth` can not be replicated.
      1. `..$smth` means an arbitrary mix of `$smth` and `..$smth` unquote templates separated according to their location (e.g. an empty string, `[$tpe]`, `[..$tpes, $tpe]` all conform to `[..$tpes]`, and the separator is a comma, as appropriate for a list of type arguments).
      1. `...$smth` means an arbitrary mix of `$smth`, `..$smth` and  `...$smth` unquote templates, separated accordingly.
      1. If a suffix of `smth` says that it's a non-empty sequence, then replication can't result in an empty list.
      1. If a quasiquote is used as a pattern, then some replications may be illegal (TODO: to be elaborated!).

    1. Finally, we match the unquotes after replication against the corresponding parts of the quasiquote under conformance test. There are three possibilities for a match:
    <table>
      <th>
        <td width="40%">
          Description
        </td>
        <td width="60%">
          Examples:
          <br/>
          1) <code>$qname</code> in <code>q"$qname.this"</code> (<code>qname</code> means <code>Name.Qualifier</code>)
          <br/>
          2) <code>..$tpesnel</code> in <code>q"$expr[..$tpesnel]"</code> (<code>tpes</code> is a plural of <code>tpe</code>, and <code>tpe</code> means <code>Type</code>)
          <br/>
          3) <code>...$aexprssnel</code> in <code>q"$expr(...$aexprssnel)"</code> (<code>aexprssnel</code> is a plural of <code>aexpr</code>, and <code>aexpr</code> means <code>Term.Arg</code>)
        </td>
      </th>
      <tr>
        <td>
          Syntax
        </td>
        <td>
          Scala syntax corresponding to the expected type
        </td>
        <td>
          1) Any identifier. The rank of the unquote is 0, so we couldn't have replicated anything - only a single identifier is accepted,
          and it can't be omitted.
          <br/>
          2) Any type. The rank of the unquote is 1, so we could have either written a single type or multiple types separated by commas.
          <br/>
          3) Any term as well as the special <code>term*</code> and <code>name = term</code> forms will do. The rank of the unquote is 2, so we have a multitude of options that include: no aexprs, a single aexpr, multiple aexprs separated by a comma, multiple argument lists.
        </td>
      </tr>
      <tr>
        <td>
          Unquote
        </td>
        <td>
          An unquote of an object that has exactly the expected type
        </td>
        <td>
          1) <code>$x</code>, where <code>x</code> is of type <code>Name.Qualifier</code>.
          <br/>
          2) <code>..$xs</code>, where <code>xs</code> is of type <code>Seq[Type]</code>.
          <br/>
          3) <code>...$xss</code>, where <code>xss</code> is of type <code>Seq[Seq[Type]]</code>.
        </td>
      </tr>
      <tr>
        <td>
          Lifted unquote
        </td>
        <td>
          An unquote of an object of type <code>O</code> that can be lifted (a <code>Lift[O, I]</code> typeclass instance) or unlifted (a <code>Unlift[I, O]</code> typeclass instance) to the expected type <code>I</code>. The syntax is different for construction and deconstruction.
        </td>
        <td>
          Construction:
          <br/>
          1) <code>$x</code>, where there exists an implicit of type <code>Lift[X, Name.Qualifier]</code>.
          <br/>
          2) <code>..$xs</code>, where there exists an implicit of type <code>Lift[X, Seq[Type]]</code>.
          <br/>
          3) <code>..$xss</code>, where there exists an implicit of type <code>Lift[X, Seq[Seq[Type]]]</code>.
          <br/><br/>

          Deconstruction (<code>X</code> needs to be specified explicitly, because there are no expected types in pattern matching):
          <br/>
          1) <code>${x: X}</code>, where there exists an implicit of type <code>Unlift[Name.Qualifier, X]</code>.
          <br/>
          2) <code>..${xs: X}</code>, where there exists an implicit of type <code>Unlift[Seq[Type], X]</code>.
          <br/>
          3) <code>..${xss: X}</code>, where there exists an implicit of type <code>Unlift[Seq[Seq[Type]], X]</code>.
        </td>
      </tr>
    </table>

  1. If not specified explicitly, quasiquote templates work for both construction and deconstruction. In some cases, a template is only applicable to construction (e.g. it's impossible to pattern match a name without specifying an expected type explicitly, because patterns like in `term match { case q"$name" => }` will match any term, not limited to just term names).

### Shorthands

 Type                     | Shorthand     | Interpolator
--------------------------|---------------|--------------
 scala.Boolean            | `$bool`       | -
 scala.Byte               | `$byte`       | -
 scala.Char               | `$char`       | -
 scala.Double             | `$double`     | -
 scala.Float              | `$float`      | -
 scala.Int                | `$int`        | -
 scala.Long               | `$long`       | -
 scala.Short              | `$short`      | -
 scala.String             | `$string`     | -
 scala.Symbol             | `$symbol`     | -
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
 meta.Pat.Arg             | `$apat`       | `parg`
 meta.Pat.Var.Term        | `$pname`      | `p`
 meta.Pat.Var.Type        | `$ptname`     | `pt`
 meta.Pat.Type            | `$ptpe`       | `pt`
 meta.Importee            | `$importee`   | `importee`
 meta.Stat                | `$stat`       | `q`
 meta.Template            | `$template`   | `template`
 meta.Term                | `$expr`       | `q`
 meta.Term.Arg            | `$aexpr`      | `arg`
 meta.Term.Name           | `$name`       | `q`
 meta.Term.Ref            | `$ref`        | `q`
 meta.Term.Param          | `$param`      | `param`
 meta.Term.Param.Name     | `$paramname`  | `q`, anonymous names can't be constructed, only extracted from `param`
 meta.Type                | `$tpe`        | `t`
 meta.Type.Arg            | `$atpe`       | `targ`
 meta.Type.Name           | `$tname`      | `t`
 meta.Type.Param          | `$tparam`     | `tparam`
 meta.Type.Param.Name     | `$tparamname` | `t`, anonymous names can't be constructed, only extracted from `tparam`
                          | `$lit`        | `q`

### Suffixes

 Suffix | Wrapped Type  | Example
--------|---------------|-----------------------------
 -s     | `Seq[_]`      | `exprs: Seq[meta.Term]`
 -ss    | `Seq[Seq[_]]` | `exprss: Seq[Seq[meta.Term]]`
 -opt   | `Option[_]`   | `expropt: Option[meta.Term]`
 -nel   | `_`           | `tpesnel: Seq[meta.Type]`
