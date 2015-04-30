## Literals

         | Quasiquote
---------|------------------------------
 Boolean | `q"true"`, `q"false"`, `q"$bool"`
 Byte    | `q"$byte"`
 Short   | `q"$short"`
 Int     | `q"1"`, `q"$int"`
 Long    | `q"1L"`, `q"$long"`
 Float   | `q"1.0f"`, `q"$float"`
 Double  | `q"1.0"`, `q"1.0d"`, `q"$double"`
 Char    | `q" 'c' "`, `q"$char"`
 String  | `q""" "s" """`, `q"$str"`
 Symbol  | `q" 's "`, `q"$symbol"`
 Null    | `q"null"`
 Unit    | `q"()"`

## Expressions (meta.Term)

                   | Quasiquote
-------------------|------------------
 This              | `q"this"`, `q"$qname.this"`
 Super             | `q"super"`, `q"$qname.super"`, `q"super[$qname]"`, `q"$qname.super[$qname]"`
 Name              | `q"name"`
 Selection         | `q"$expr.$name"`
 Interpolation     | `q""" $name"$${..$exprs}" """`
 Application       | `q"$expr(..$aexprs)"`
 Type Application  | `q"$expr[..$tpes]"`
 Infix Application | `q"$expr $name[..$tpes] (..$aexprs)"`
 Unary Application | `q"!$expr", q"~$expr", q"-$expr", "+$expr"`
 Assign            | `q"$ref = $expr"`
 Update            | `q"$expr(..$aexprs) = $expr"`
 Return            | `q"return $expropt"`
 Throw             | `q"throw $expr"`
 Ascribe           | `q"$expr: $tpe"`
 Annotate          | `q"$expr: ..@$expr"`
 Tuple             | `q"(..$exprs)"`
 Block             | `q"{ ..$stats }"`
 If                | `q"if ($expr) $expr else $expr"`
 Match             | `q"$expr match { ..case $cases }"`
 Try Catch Cases   | `q"try $expr catch { ..case $cases } finally $expropt"`
 Try Catch Expr    | `q"try $expr catch $expr finally $expropt" `
 Function          | `q"(..$params) => $expr"`
 Partial Function  | `q"{ ..case $cases }"`
 While             | `q"while ($expr) $expr"`
 Do While          | `q"do $expr while($expr)"`
 For               | `q"for (..$enumerators) $expr"`
 For Yield         | `q"for (..$enumerators) yield $expr"`
 New               | `q"new $template"`
 Placeholder       | `q"_"`
 Eta Expansion     | `q"$expr _"`
 Literal           | `q"$lit"`

## Arguments (meta.Term.Arg)

            | Quasiquote
------------|------------------------------
 Named      | `arg"$name = $expr"`
 Repeated   | `arg"$expr: _*"`
 Expression | `arg"$expr"`

## Types (meta.Type)

                   | Quasiquote
-------------------|------------------------------
 Name              | `t"name"`
 Selection         | `t"$ref.$tname"`
 Projection        | `t"$tpe#$tname"`
 Singleton         | `t"$ref.type"`
 Application       | `t"$tpe[..$tpes]`
 Infix Application | `t"$tpe $tname $tpe"`
 Function          | `t"(..$atpes) => $tpe"`
 Tuple             | `t"(..$tpes)"`
 Compound          | `t"..$tpes { ..$stats }"`
 Existential       | `t"$tpe forSome { ..$stats }"`
 Annotate          | `t"$tpe ..@$expr"`
 Placeholder       | `t"_ >: $tpeopt <: tpeopt"`
 Literal           | `t"$lit"`

## Argument Types (meta.Type.Arg)

          | Quasiquote
----------|-----------------
 By Name  | `t"=> $tpe"`
 Repeated | `t"$tpe *"`
 Type     | `t"$tpe"`

## Patterns (meta.Pat) and Cases (meta.Case)

               | Quasiquote
---------------|----------------------------
 Wildcard      | `p"_"`
 Var           | `p"name"`
 Bind          | `p"$name @ $pat"`
 Alternative   | `p"$pat | $pat"`
 Tuple         | `p"(..$pats)"`
 Extract       | `p"$ref[..$tpes](..$apats)"`
 Infix Extract | `p"$pat $name (..$apats)"`
 Interpolation | `p""" $name"$${..$pats}" """`
 Typed         | `p"$pat: $ptpe"`
 Name          | ``p"`name`"``
 Selection     | `p"$expr.$name"`
 Literal       | `p"$lit"`
 Case          | `p"case $pat if $condopt => $expr"`

## Argument Patterns (meta.Pat.Arg)

                   | Quasiquote
-------------------|----------------------------
 Sequence Wildcard | `p"_*"`
 Pattern           | `p"$pat"`

## Type Patterns (meta.Pat.Type)

                   | Quasiquote
-------------------|------------------------------
 Wildcard          | `pt"_"`
 Var               | `pt"name"`
 Name              | ``pt"`name`"``
 Selection         | `pt"$ref.$tname"`
 Projection        | `pt"$ptpe#$tname"`
 Singleton         | `pt"$ref.type"`
 Application       | `pt"$ptpe[..$ptpes]`
 Infix Application | `pt"$ptpe $tname $ptpe"`
 Function          | `pt"(..$ptpes) => $ptpe"`
 Tuple             | `pt"(..$ptpes)"`
 Compound          | `pt"..$ptpes { ..$stats }"`
 Existential       | `pt"$ptpe forSome { ..$stats }"`
 Annotate          | `pt"$ptpe ..@$expr"`
 Placeholder       | `pt"_ >: $tpeopt <: tpeopt"`
 Literal           | `pt"$lit"`

## Statements (meta.Stat)

            | Quasiquote
------------|----------------------------
 Expression | `q"$expr"`
 Member     | `q"$member"`
 Import     | `q"import ..($ref.{..$importees})"`

## Members (meta.Member)

### Declarations

           | Quasiquote
-----------|------------------------------
 Val       | `q"..$mods val ..$names: $tpe"`
 Var       | `q"..$mods var ..$names: $tpe"`
 Def       | `q"..$mods def $name[..$tparams](...$paramss): $tpe"`
 Type      | `q"..$mods type $tname[..$tparams] >: $tpeopt <: tpeopt"`

### Definitions

                | Quasiquote
----------------|------------------------------
 Val            | `q"..$mods val ..$pats: $tpeopt = $expr"`
 Var            | `q"..$mods var ..$pats: $tpeopt = $expropt"`
 Def            | `q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr"`
 Macro          | `q"..$mods def $name[..$tparams](...$paramss): $tpe = macro $expr"`
 Type           | `q"..$mods type $tname[..$tparams] = $tpe"`
 Class          | `q"..$mods class $tname[..$tparams] $member extends $template"`
 Trait          | `q"..$mods trait $tname[..$tparams] extends $template"`
 Object         | `q"..$mods object $name extends $template"`
 Package Object | `q"package object $name extends $template"`
 Package        | `q"package $ref { ..$stats }"`
 Primary Ctor   | `q"..$mods def this(..$paramss)"`
 Secondary Ctor | `q"..$mods def this(..$paramss) = $expr"`

### Value Parameters (meta.Term.Param)

                | Quasiquote
----------------|-------------------------------------------------
 Term Param     | `param"..$mods $pname: $atpeopt = $defaultopt"`

### Type Parameters (meta.Type.Param)

                | Quasiquote
----------------|-------------------------------------------------
 Type Param     | `tparam"..$mods $tpname[..$tparams] >: $tpeopt <: $tpeopt <% ..$tpes : ..$tpes"`

## Constructor References (meta.Ctor.Ref and meta.Term)

                     | Quasiquote
---------------------|------------------------------
 Name Reference      | `ctor"$ctorname"`
 Select Reference    | `ctor"$ref.$ctorname"`
 Project Reference   | `ctor"$tpe#$ctorname"`
 Function Reference  | `ctor"(..$tpes) => $tpe"`
 Annotated Reference | `ctor"$ctorname ..@$expr"`
 Applied Reference   | `ctor"$ctorref(...$aexprss)"`
 Tapplied Reference  | `ctor"$ctorref[..$atpes]"`

## Template (meta.Template)

           | Quasiquote
-----------|--------------------
 Template  | `template"{ ..$stat } with ..$exprs { $param => ..$stats }"`

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
 Name      | `importee"$iname"`
 Rename    | `importee"$iname => $iname"`
 Unimport  | `importee"$iname => _"`
 Wildcard  | `importee"_"`

## Sources (meta.Source)

           | Quasiquote
-----------|---------------------------
 Source    | `source"..$stats"`

## Naming conventions

### Shorthands and interpolators

 Type                     | Shorthand     | Interpolator
--------------------------|---------------|--------------
 meta.Case                | `$case`       | `p`
 meta.Ctor.Name           | `$ctorname`   | `ctor`
 meta.Ctor.Ref            | `$ctorref`    | `ctor`
 meta.Enumerator          | `$enumerator` | `enumerator`
 meta.Member              | `$member`     | `q`
 meta.Mod                 | `$mod`        | `mod`
 meta.Name.Indeterminate  | `$iname`      | Can't be constructed, only extracted from `importee"..."` and `mod"..."`
 meta.Name.Qualifier      | `$qname`      | `q`, `t`, anonymous names can't be constructed, only extracted from `mod"..."`
 meta.Pat                 | `$pat`        | `p`
 meta.Pat.Arg             | `$apat`       | `p`
 meta.Pat.Type            | `$ptpe`       | `pt`
 meta.Importee            | `$importee`   | `importee`
 meta.Stat                | `$stat`       | `q`
 meta.Template            | `$template`   | `template`
 meta.Term                | `$expr`       | `q`
 meta.Term.Arg            | `$aexpr`      | `arg`
 meta.Term.Name           | `$name`       | `q`
 meta.Term.Ref            | `$ref`        | `q`
 meta.Term.Param          | `$param`      | `param`
 meta.Term.Param.Name     | `$pname`      | `q`, anonymous names can't be constructed, only extracted from `param`
 meta.Type                | `$tpe`        | `t`
 meta.Type.Arg            | `$atpe`       | `t`
 meta.Type.Name           | `$tname`      | `t`
 meta.Type.Param          | `$tparam`     | `tparam`
 meta.Type.Param.Name     | `$tpname`     | `t`, anonymous names can't be constructed, only extracted from `tparam`
                          | `$lit`        | `q`

### Suffix name modifiers

 Suffix | Wrapped Type  | Example
--------|---------------|-----------------------------
 -s     | `Seq[_]`      | `exprs: Seq[meta.Term]`
 -ss    | `Seq[Seq[_]]` | `exprss: Seq[Seq[meta.Term]]`
 -opt   | `Option[_]`   | `expropt: Option[meta.Term]`
