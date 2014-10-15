## Types, interpolators and naming conventions

### Core types and interpolators

                   | Interpolator  | Shorthand
-------------------|---------------|-----------
 meta.Decl         | `q`           | `$decl`
 meta.Defn         | `q`           | `$defn`
 meta.Lit          | `q`           | `$lit`
 meta.Mod          | `mod`         | `$mod`
 meta.Mod.Annot    | `mod`         | `$annot`
 meta.Name         |               |
 meta.Pat          | `p`           | `$pat`
 meta.Stat         | `q`           | `$stat`
 meta.Templ        | `templ`       | `$templ`
 meta.Templ.Parent | `templ`       | `$parent`
 meta.Term         | `q`           | `$expr`
 meta.Term.Name    | `q`           | `$name`
 meta.Term.Ref     | `q`           | `$ref`
 meta.Tree         |               | 
 meta.Type         | `t`           | `$tpe`
 meta.Type.Name    | `t`           | `$tname`
 meta.Type.Ref     | `t`           | `$tref`

### Auxilary types and interpolators

                          | Interpolator | Shorthand
--------------------------|--------------|-----------
 meta.aux.Arg             | `arg`        | `$arg`
 meta.aux.Case            | `cas`        | `$cas`
 meta.aux.CompUnit        | `unit`       | `$unit`
 meta.aux.Enum            | `enum`       | `$enum`
 meta.aux.Import.Clause   | `imp`        | `$iclause`
 meta.aux.Import.Name     | `imp`        | `$iname`
 meta.aux.Import.Selector | `imp`        | `$imp`
 meta.aux.Param           | `param`      | `$param`
 meta.aux.Param.Type      | `t`          | `$paramtpe`
 meta.aux.Qual.Access     | `qual`       | `$aqual`
 meta.aux.Qual.Name       | `qual`       | `$qname`
 meta.aux.Qual.Term       | `qual`       | `$equal`
 meta.aux.Qual.Type       | `qual`       | `$tqual`
 meta.aux.TypeParam       | `tparam`     | `$tparam`

### Suffix name modifiers

 Suffix | Wrapped Type  | Example
--------|---------------|-----------------------------
 -s     | `Seq[_]`      | `exprs: Seq[meta.Term]`
 -ss    | `Seq[Seq[_]]` | `exprss: Seq[Seq[meta.Term]]`
 -opt   | `Option[_]`   | `expropt: Option[meta.Term]`

## Literals (meta.Lit)

                   | Quasiquote
-------------------|------------------------------
 Boolean           | `q"true"`, `q"false"`, `q"$bool"`
 Int               | `q"1"`, `q"$int"`
 Long              | `q"1L"`, `q"$long"`
 Float             | `q"1.0"`, `q"$float"`
 Double            | `q"1.0d"`, `q"$double"`
 Char              | `q" 'c' "`, `q"$char"`
 String            | `q""" "s" """`, `q"$string"`
 Symbol            | `q" 's "`, `q"$symbol"`
 Null              | `q"null"`
 Unit              | `q"()"`

## Expressions (meta.Term)

                   | Quasiquote                                                       
-------------------|------------------
 Name              | `q"name"`
 Literal           | `q"$lit"`
 Interpolation     | `q""" $name"$${..$exprs}" """`
 This              | `q"$qnameopt.this"`
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
 Annotated         | `q"$expr: ..@$annots" `
 Tuple             | `q"(..$exprs)"`
 Block             | `q"{ ..$stats }"`
 If                | `q"if ($expr) $expr else $expr"`
 Match             | `q"$expr match { ..case $pat if $cond => ..$stat }"`
 Try Catch Cases   | `q"try $expr catch $expr finally $expr"`
 Try Catch Expr    | `q"try $expr catch { ..case $pat if $cond => ..$stat } finally $expr" `
 Function          | `q"(..$params) => $expr"`
 Partial Function  | `q"{ case ..$cass }"`
 While             | `q"while ($expr) $expr"`
 Do While          | `q"do $expr while($expr)"`
 For               | `q"for (..$enums) $expr"`
 For Yield         | `q"for (..$enums) yield $expr"`
 New               | `q"new $templ"`
 Placeholder       | `q"_"`
 Eta Expansion     | `q"$expr _"`

## Case (meta.Case)

        | Quasiquote
--------|---------
 Case   | `cas"$pat if $term => ..$stats"`

## Enumerators (meta.Enum)

           | Quasiquote                                                       
-----------|------------------------------
 Generator | `enum"$pat <- $expr"`
 Value     | `enum"$pat = $expr"`
 Guard     | `enum"if $expr"`

## Arguments (meta.Arg)

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
 Selection         | `t"$tqual.$tname"`
 Projection        | `t"$tpe#$tname"`
 Singleton         | `t"$tqual.type"`
 Application       | `t"$tpe[..$tpes]`
 Infix Application | `t"$tpe $tpe $tpe"`
 Function          | `t"(..$paramtpes) => $tpe"`
 Tuple             | `t"(..$tpes)"`
 Compound          | `t"..$tpes { ..$stats }"`
 Existential       | `t"$tpe forSome { ..$stats }"`
 Annotated         | `t"$tpe ..@$annots"`
 Placeholder       | `t"_"`

## Param Types (meta.Param.Type)

          | Quasiquote
----------|-----------------
 By Name  | `t"=> $tpe"`
 Repeated | `t"$tpe *"`
 Type     | `t"$tpe"`

## Params (meta.Param)

           | Quasiquote
-----------|-----------------
 Anonymous | `param"..$mods _: $paramtpe"`
 Named     | `param"..$mods $name: $paramtpe = $default"`

## Type Params (meta.TypeParam)

           | Quasiquote
-----------|-----------------
 Anonymous | `tparam"..$mods _[..$tparams] <% ..$tpes : ..$tpes >: $tpeopt <: $tpeopt"`
 Named     | `tparam"..$mods $name[..$tparams] <% ..$tpes : ..$tpes >: $tpeopt <: $tpeopt"`

## Patterns (meta.Pat)

                   | Quasiquote
-------------------|----------------------------
 Name              | `p"name"`
 Literal           | `p"$lit"`
 Interpolation     | `p""" $name"$${..$pats}" """`
 Wildcard          | `p"_"`
 Sequence Wildcard | `p"_*"`
 Binding           | `p"$name @ $pat"`
 Alternative       | `p"$pat | $pat"`
 Tuple             | `p"(..$pats)"`
 Extract           | `p"$ref[..$tpes](..$pats)"`
 Infix Extract     | `p"$pat $ref (..$pats)"`
 Type              | `p"$pat: $tpe"`

## Statements (meta.Stat)

                    | Quasiquote
--------------------|----------------------------
 Declaration        | `q"$decl"`
 Definition         | `q"$defn"`
 Package            | `q"package $ref { ..$stats }"`
 Import             | `q"import ..$iclauses"`
 Comment            | `q"//$string"`
 Multi-Line Comment | `q"/*$string*/"`

## Import Clauses (meta.Import.Clause) and Selectors (meta.Import.Selector)

                   | Quasiquote
-------------------|---------------------------
 Import Clause     | `imp"$ref.{..$imps}"`
 Name Selector     | `imp"name"`
 Rename Selector   | `imp"$iname => $iname"`
 Unimport Selector | `imp"$iname => _"`
 Wildcard Selector | `imp"_"`

## Declarations (meta.Decl)

           | Quasiquote
-----------|------------------------------
 Val       | `q"..$mods val $name: $tpe"`
 Var       | `q"..$mods var $name: $tpe"`
 Def       | `q"..$mods def $name[..$tparams](...$paramss): $tpe"`
 Procedure | `q"..$mods def $name[..$tparams](...$paramss)"`
 Type      | `q"..$mods type $tname[..$tparams] >: $tpeopt <: tpeopt"`

## Definitions (meta.Defn)

                | Quasiquote
----------------|------------------------------
 Val            | `q"..$mods val $name: $tpe = $expr"`
 Var            | `q"..$mods var $name: $tpe = $expr", q"..$mods var $name: $tpe = _"`
 Def            | `q"..$mods def $name[..$tparams](...$paramss): $tpe = $expr"`
 Macro          | `q"..$mods def $name[..$tparams](...$paramss): $tpe = macro $expr"`
 Procedure      | `q"..$mods def $name[..$tparams](...$paramss) { ..$stats }"`
 Secondary Ctor | `q"..$mods def this(..$paramss) = { this(...$args); ..$stats }"`
 Type           | `q"..$mods type $tname[..$tparams] >: $tpeopt <: tpeopt"`
 Class          | `q"..$mods class $tname[..$tparams] $cmods(...$paramss) extends $template"`
 Trait          | `q"..$mods trait $tname[..$tparams] extends $template"`
 Object         | `q"..$mods object $name[..$tparams] extends $template"`


## Template, Parents and Self
           
           | Quasiquote
-----------|--------------------
 Template  | `templ"{ ..$stat } with ..$parents { $nameopt: $tpeopt => ..$stats }"`
 Parent    | `templ"$tpe(...$argss)"`

## Modifiers (meta.Mod) and Annotations (meta.Annot)

               | Quasiquote
---------------|-----------------
 Annotation    | `mod"@$tpe(..$argss)"`
 Private       | `mod"private[$qualaopt]"`
 Protected     | `mod"protected[$qualaopt]"`
 Implicit      | `mod"implicit"`
 Final         | `mod"final"`
 Sealed        | `mod"sealed"`
 Override      | `mod"override"`
 Case          | `mod"case"`
 Abstract      | `mod"abstract"`
 Covariant     | `mod"+"`
 Contravariant | `mod"-"`
 Lazy          | `mod"lazy"`
 Val           | `mod"val"`
 Var           | `mod"var"`
 Package       | `mod"package"`

## Names (meta.*.Name)

             | Quasiquote
-------------|-------------
 Term Name   | `q"name"`
 Type Name   | `t"name"`
 Qual Name   | `qual"name"`
 Import Name | `imp"name"`
 
## References (meta.Ref.*)

                 | Ref.Term? | Ref.Type?
-----------------|-----------|-----------
 Term Name       | +         | –
 Term Selection  | +         | –
 Term This       | +         | – 
 Type Name       | –         | +
 Type Selection  | –         | +
 Type Projection | –         | +

## Qualifiers (meta.Qual.*)

       | Quasiquote
-------|---------------------------------
 Super | `qual"$qnameopt.super[$tnameopt]"`
 Name  | `qual"name"`

           | Qual.Term? | Qual.Type? | Qual.Access?
-----------|------------|------------|--------------
 Term      | +          | –          | –
 This      | +          | +          | +
 Ref.Term  | +          | +          | –
 Super     | +          | +          | –
 Qual.Name | –          | –          | +
 
## Compilation Units (meta.CompUnit)

                  | Quasiquote
------------------|----------------------------------
 Compilation Unit | `cunit"package ..$refs; ..$stats"`
