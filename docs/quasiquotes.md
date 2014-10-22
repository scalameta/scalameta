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
 Name              | `q"name"`
 Literal           | `q"$lit"`
 Interpolation     | `q""" $name"$${..$exprs}" """`
 This              | `q"$stropt.this"`
 Super             | `q"$stropt.super[$tnameopt]"`
 Selection         | `q"$expr.$name"`
 Application       | `q"$expr(..$args)"`
 Type Application  | `q"$expr[..$tpes]"`
 Infix Application | `q"$expr $name[..$tpes] (..$args)"`
 Unary Application | `q"!$expr", q"~$expr", q"-$expr", "+$expr"`
 Assign            | `q"$ref = $expr"`
 Update            | `q"$expr(..$args) = $expr"`
 Return            | `q"return $expropt"`
 Throw             | `q"throw $expr"`
 Ascribe           | `q"$expr: $tpe"`
 Annotate          | `q"$expr: ..@$parents"`
 Tuple             | `q"(..$exprs)"`
 Block             | `q"{ ..$stats }"`
 If                | `q"if ($expr) $expr else $expr"`
 Match             | `q"$expr match { ..case $pat if $cond => ..$stat }"`
 Try Catch Cases   | `q"try $expr catch $expr finally $expr"`
 Try Catch Expr    | `q"try $expr catch { ..case $pat if $cond => ..$stat } finally $expr" `
 Function          | `q"(..$params) => $expr"`
 Partial Function  | `q"{ ..case $pat if $cond => ..$stat }"`
 While             | `q"while ($expr) $expr"`
 Do While          | `q"do $expr while($expr)"`
 For               | `q"for (..$enums) $expr"`
 For Yield         | `q"for (..$enums) yield $expr"`
 New               | `q"new $templ"`
 Placeholder       | `q"_"`
 Eta Expansion     | `q"$expr _"`

## Arguments (meta.Term.Arg)

          | Quasiquote                                                       
----------|------------------------------
 Named    | `arg"$name = $expr"`
 Repeated | `arg"$expr: _*"`
 Term     | `arg"$expr"`

## Types (meta.Type)

                   | Quasiquote
-------------------|------------------------------
 Name              | `t"name"`
 Literal           | `t"$lit"`
 Selection         | `t"$ref.$tname"`
 Projection        | `t"$tpe#$tname"`
 Singleton         | `t"$ref.type"`
 Application       | `t"$tpe[..$tpes]`
 Infix Application | `t"$tpe $tpe $tpe"`
 Function          | `t"(..$atpes) => $tpe"`
 Tuple             | `t"(..$tpes)"`
 Compound          | `t"..$tpes { ..$stats }"`
 Existential       | `t"$tpe forSome { ..$stats }"`
 Annotate          | `t"$tpe ..@$parents"`
 Placeholder       | `t"_"`

## Argument Types (meta.Type.Arg)

          | Quasiquote
----------|-----------------
 By Name  | `t"=> $tpe"`
 Repeated | `t"$tpe *"`
 Type     | `t"$tpe"`

## Patterns (meta.Pat)

               | Quasiquote
---------------|----------------------------
 Name          | `p"name"`
 Literal       | `p"$lit"`
 Interpolation | `p""" $name"$${..$pats}" """`
 Wildcard      | `p"_"`
 Binding       | `p"$name @ $pat"`
 Alternative   | `p"$pat | $pat"`
 Tuple         | `p"(..$pats)"`
 Extract       | `p"$ref[..$tpes](..$apats)"`
 Infix Extract | `p"$pat $ref (..$apats)"`
 Type          | `p"$pat: $tpe"`

## Argument Patterns (meta.Pat.Arg)

                   | Quasiquote
-------------------|----------------------------
 Pattern           | `p"$pat"`
 Sequence Wildcard | `p"_*"`

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
 Val       | `q"..$mods val $name: $tpe"`
 Var       | `q"..$mods var $name: $tpe"`
 Def       | `q"..$mods def $name[..$tparams](...$paramss): $tpe"`
 Procedure | `q"..$mods def $name[..$tparams](...$paramss)"`
 Type      | `q"..$mods type $tname[..$tparams] >: $tpeopt <: tpeopt"`

### Definitions

                | Quasiquote
----------------|------------------------------
 Val            | `q"..$mods val $name: $tpe = $expr"`
 Var            | `q"..$mods var $name: $tpe = $expr", q"..$mods var $name: $tpe = _"`
 Def            | `q"..$mods def $name[..$tparams](...$paramss): $tpe = $expr"`
 Macro          | `q"..$mods def $name[..$tparams](...$paramss): $tpe = macro $expr"`
 Procedure      | `q"..$mods def $name[..$tparams](...$paramss) { ..$stats }"`
 Primary Ctor   | `q"..$mods def this(..$paramss)"`
 Secondary Ctor | `q"..$mods def this(..$paramss) = { this(...$args); ..$stats }"`
 Type           | `q"..$mods type $tname[..$tparams] = $tpe"`
 Class          | `q"..$mods class $tname[..$tparams] ..$mods(...$paramss) extends $template"`
 Trait          | `q"..$mods trait $tname[..$tparams] extends $template"`
 Object         | `q"..$mods object $name extends $template"`
 Package Object | `q"package object $name extends $template"`
 Package        | `q"package $ref { ..$stats }"`

### Params (meta.Param)

           | Quasiquote
-----------|-----------------
 Anonymous | `param"..$mods _: $atpe"`
 Named     | `param"..$mods $name: $atpe = $default"`

### Type Params (meta.TypeParam)

           | Quasiquote
-----------|-----------------
 Anonymous | `tparam"..$mods _[..$tparams] <% ..$tpes : ..$tpes >: $tpeopt <: $tpeopt"`
 Named     | `tparam"..$mods $name[..$tparams] <% ..$tpes : ..$tpes >: $tpeopt <: $tpeopt"`

## Template, Parents and Self
           
           | Quasiquote
-----------|--------------------
 Template  | `templ"{ ..$stat } with ..$parents { $param => ..$stats }"`
 Parent    | `templ"$tpe(...$argss)"`

## Modifiers (meta.Mod)

                  | Quasiquote
------------------|-----------------
 Annotation       | `mod"@$parent"`
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

## Naming conventions

### Shorthands and interpolators

 Type           | Shorthand | Interpolator 
----------------|-----------|--------------
 meta.Enum      | `$enum`   | `enum`       
 meta.Member    | `$memb`   | `q`          
 meta.Mod       | `$mod`    | `mod`        
 meta.Param     | `$param`  | `param`      
 meta.Parent    | `$parent` | `templ`      
 meta.Pat       | `$pat`    | `p`          
 meta.Pat.Arg   | `$apat`   | `p`          
 meta.Selector  | `$sel`    | `sel`        
 meta.Stat      | `$stat`   | `q`          
 meta.Templ     | `$templ`  | `templ`      
 meta.Term      | `$expr`   | `q`          
 meta.Term.Arg  | `$arg`    | `arg`        
 meta.Term.Name | `$name`   | `q`          
 meta.Term.Ref  | `$ref`    | `q`          
 meta.Type      | `$tpe`    | `t`          
 meta.Type.Arg  | `$atpe`   | `t`          
 meta.Type.Name | `$tname`  | `t`          
 meta.TypeParam | `$tparam` | `tparam`     
                | `$lit`    | `q` 

### Suffix name modifiers

 Suffix | Wrapped Type  | Example
--------|---------------|-----------------------------
 -s     | `Seq[_]`      | `exprs: Seq[meta.Term]`
 -ss    | `Seq[Seq[_]]` | `exprss: Seq[Seq[meta.Term]]`
 -opt   | `Option[_]`   | `expropt: Option[meta.Term]`
