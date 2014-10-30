## Literals

         | Quasiquote
---------|------------------------------
 Boolean | `q"true"`, `q"false"`, `q"$bool"`
 Int     | `q"1"`, `q"$int"`
 Long    | `q"1L"`, `q"$long"`
 Float   | `q"1.0"`, `q"$float"`
 Double  | `q"1.0d"`, `q"$double"`
 Char    | `q" 'c' "`, `q"$char"`
 String  | `q""" "s" """`, `q"$str"`
 Symbol  | `q" 's "`, `q"$symbol"`
 Null    | `q"null"`
 Unit    | `q"()"`

## Expressions (meta.Term)

                   | Quasiquote
-------------------|------------------
 This              | `q"$stropt.this"`
 Super             | `q"$stropt.super[$stropt]"`
 Name              | `q"name"`
 Selection         | `q"$expr.$name"`
 Interpolation     | `q""" $name"$${..$exprs}" """`
 Application       | `q"$expr(..$args)"`
 Type Application  | `q"$expr[..$tpes]"`
 Infix Application | `q"$expr $name[..$tpes] (..$args)"`
 Unary Application | `q"!$expr", q"~$expr", q"-$expr", "+$expr"`
 Assign            | `q"$ref = $expr"`
 Update            | `q"$expr(..$args) = $expr"`
 Return            | `q"return $expropt"`
 Throw             | `q"throw $expr"`
 Ascribe           | `q"$expr: $tpe"`
 Annotate          | `q"$expr: ..@$crefs"`
 Tuple             | `q"(..$exprs)"`
 Block             | `q"{ ..$stats }"`
 If                | `q"if ($expr) $expr else $expr"`
 Match             | `q"$expr match { case ..$cass }"`
 Try Catch Cases   | `q"try $expr catch { case ..$cass } finally $expropt"`
 Try Catch Expr    | `q"try $expr catch $expr finally $expropt" `
 Function          | `q"(..$params) => $expr"`
 Partial Function  | `q"{ case ..$cass }"`
 While             | `q"while ($expr) $expr"`
 Do While          | `q"do $expr while($expr)"`
 For               | `q"for (..$enums) $expr"`
 For Yield         | `q"for (..$enums) yield $expr"`
 New               | `q"new $templ"`
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
 Annotate          | `t"$tpe ..@$crefs"`
 Placeholder       | `t"_ >: $tpeopt <: tpeopt"`
 Literal           | `t"$lit"`

## Argument Types (meta.Type.Arg)

          | Quasiquote
----------|-----------------
 By Name  | `t"=> $tpe"`
 Repeated | `t"$tpe *"`
 Type     | `t"$tpe"`

## Patterns (meta.Pat)

               | Quasiquote
---------------|----------------------------
 Wildcard      | `p"_"`
 Bind          | `p"$name @ $pat"`
 Alternative   | `p"$pat | $pat"`
 Tuple         | `p"(..$pats)"`
 Extract       | `p"$ref[..$tpes](..$apats)"`
 Infix Extract | `p"$pat $name (..$apats)"`
 Interpolation | `p""" $name"$${..$pats}" """`
 Typed         | `p"$pat: $tpe"`
 Name          | `p"name"`
 Selection     | `p"$expr.$name"`
 Literal       | `p"$lit"`

## Argument Patterns (meta.Pat.Arg)

                   | Quasiquote
-------------------|----------------------------
 Sequence Wildcard | `p"_*"`
 Pattern           | `p"$pat"`

## Statements (meta.Stat)

            | Quasiquote
------------|----------------------------
 Expression | `q"$expr"`
 Member     | `q"$memb"`
 Import     | `q"import ..($ref.{..$sels})"`

## Members (meta.Member)

### Declarations

           | Quasiquote
-----------|------------------------------
 Val       | `q"..$mods val ..$names: $tpe"`
 Var       | `q"..$mods var ..$names: $tpe"`
 Def       | `q"..$mods def $name[..$tparams](...$paramss): $tpe"`
 Procedure | `q"..$mods def $name[..$tparams](...$paramss)"`
 Type      | `q"..$mods type $tname[..$tparams] >: $tpeopt <: tpeopt"`

### Definitions

                | Quasiquote
----------------|------------------------------
 Val            | `q"..$mods val ..$pats: $tpeopt = $expr"`
 Var            | `q"..$mods var ..$pats: $tpeopt = $expropt"`
 Def            | `q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr"`
 Macro          | `q"..$mods def $name[..$tparams](...$paramss): $tpe = macro $expr"`
 Procedure      | `q"..$mods def $name[..$tparams](...$paramss) { ..$stats }"`
 Primary Ctor   | `q"..$mods def this(..$cparamss)"`
 Secondary Ctor | `q"..$mods def this(..$paramss) = { this(...$argss); ..$stats }"`
 Type           | `q"..$mods type $tname[..$tparams] = $tpe"`
 Class          | `q"..$mods class $tname[..$tparams] $ctor extends $template"`
 Trait          | `q"..$mods trait $tname[..$tparams] extends $template"`
 Object         | `q"..$mods object $name extends $template"`
 Package Object | `q"package object $name extends $template"`
 Package        | `q"package $ref { ..$stats }"`

### Params

                | Quasiquote
----------------|-------------------------------------------------
 Term Param     | `param"..$mods $nameopt: $atpeopt = $defaultopt"`
 Template Param | `param"..$mods $name: $atpe = $defaultopt"`, `param"..$mods val $name: $atpe = $defaultopt"`, `param"..$mods var $name: $atpe = $defaultopt"`
 Type Param     | `param"..$mods type $nameopt[..$tparams] <% ..$tpes : ..$tpes >: $tpeopt <: $tpeopt"`

## Template (meta.Template) and Parents (meta.Ctor.Ref)

           | Quasiquote
-----------|--------------------
 Template  | `templ"{ ..$stat } with ..$crefs { $param => ..$stats }"`
 Parent    | `templ"$tpe(...$argss)"`

## Modifiers (meta.Mod)

                  | Quasiquote
------------------|-----------------
 Annotation       | `mod"@$cref"`
 Private          | `mod"private"`
 Private Within   | `mod"private[$str]"`
 Private This     | `mod"private[this]"`
 Protected        | `mod"protected"`
 Protected Within | `mod"protected[$str]"`
 Protected This   | `mod"protected[this]"`
 Implicit         | `mod"implicit"`
 Final            | `mod"final"`
 Sealed           | `mod"sealed"`
 Override         | `mod"override"`
 Case             | `mod"case"`
 Abstract         | `mod"abstract"`
 Covariant        | `mod"+"`
 Contravariant    | `mod"-"`
 Lazy             | `mod"lazy"`

## Enumerators (meta.Enum)

           | Quasiquote
-----------|------------------------------
 Generator | `enum"$pat <- $expr"`
 Value     | `enum"$pat = $expr"`
 Guard     | `enum"if $expr"`

## Selectors (meta.Selector)

                   | Quasiquote
-------------------|---------------------------
 Name Selector     | `sel"$str"`
 Rename Selector   | `sel"$str => $str"`
 Unimport Selector | `sel"$str => _"`
 Wildcard Selector | `sel"_"`

## Cases (meta.Case)

      | Quasiquote
------|---------------------------
 Case | `cas"$pat if $condopt => ..$stat"`

## Naming conventions

### Shorthands and interpolators

 Type             | Shorthand | Interpolator
------------------|-----------|--------------
 meta.Enum        | `$enum`   | `enum`
 meta.Case        | `$cas`    | `cas`
 meta.Member      | `$memb`   | `q`
 meta.Mod         | `$mod`    | `mod`
 meta.Ctor.Ref    | `$cref`   | `templ`
 meta.Pat         | `$pat`    | `p`
 meta.Pat.Arg     | `$apat`   | `p`
 meta.Selector    | `$sel`    | `sel`
 meta.Stat        | `$stat`   | `q`
 meta.Templ       | `$templ`  | `templ`
 meta.Templ.Param | `$cparam` | `param`
 meta.Term        | `$expr`   | `q`
 meta.Term.Arg    | `$arg`    | `arg`
 meta.Term.Name   | `$name`   | `q`
 meta.Term.Ref    | `$ref`    | `q`
 meta.Term.Param  | `$param`  | `param`
 meta.Type        | `$tpe`    | `t`
 meta.Type.Arg    | `$atpe`   | `t`
 meta.Type.Name   | `$tname`  | `t`
 meta.Type.Param  | `$tparam` | `param`
                  | `$lit`    | `q`

### Suffix name modifiers

 Suffix | Wrapped Type  | Example
--------|---------------|-----------------------------
 -s     | `Seq[_]`      | `exprs: Seq[meta.Term]`
 -ss    | `Seq[Seq[_]]` | `exprss: Seq[Seq[meta.Term]]`
 -opt   | `Option[_]`   | `expropt: Option[meta.Term]`
