Below you can find a comprehensive map between Scala's language constructs and
various quasiquotes in scala.meta. If something's missing here, it's a bug that
should be
[submitted to our issue tracker](https://github.com/scalameta/scalameta/issues/new).

This specification describes quasiquote syntax using a markedly condensed
notation. If you have troubles decyphering it, consult the "Legend" section in
the end of the document.

## Expressions (meta.Term)

|                   | Quasiquote                                                               |
| ----------------- | ------------------------------------------------------------------------ |
| Literal           | `q"<literal>"`                                                           |
| This              | `q"this"`, `q"$name.this"`                                               |
| Super             | `q"super"`, `q"$name.super"`, `q"super[$name]"`, `q"$name.super[$name]"` |
| Name              | `q"<name>"`                                                              |
| Selection         | `q"$expr.$ename"`                                                        |
| Interpolation     | Not supported yet                                                        |
| Application       | `q"$expr(...$exprss)"`                                                   |
| Type Application  | `q"$expr[..$tpesnel]"`                                                   |
| Infix Application | `q"$expr $ename[..$tpes] $expr"`, `q"$expr $ename[..$tpes] (..$exprs)"`  |
| Unary Application | `q"!$expr", q"~$expr", q"-$expr", "+$expr"`                              |
| Assign            | `q"$expr = $expr"`                                                       |
| Return            | `q"return $expr"`                                                        |
| Throw             | `q"throw $expr"`                                                         |
| Ascribe           | `q"$expr: $tpe"`                                                         |
| Annotate          | `q"$expr: ..@$annotsnel"`                                                |
| Tuple             | `q"(..$exprsnel)"`                                                       |
| Block             | `q"{ ..$stats }"`                                                        |
| If                | `q"if ($expr) $expr else $expr"`                                         |
| Match             | `q"$expr match { ..case $casesnel }"`                                    |
| Try               | `q"try $expr catch { ..case $cases } finally $expropt"`                  |
| Try With Handler  | `q"try $expr catch $expr finally $expropt"`                              |
| Function          | `q"(..$params) => $expr"`                                                |
| Partial Function  | `q"{ ..case $casesnel }"`                                                |
| While             | `q"while ($expr) $expr"`                                                 |
| Do While          | `q"do $expr while($expr)"`                                               |
| For               | `q"for (..$enumeratorsnel) $expr"`                                       |
| For Yield         | `q"for (..$enumeratorsnel) yield $expr"`                                 |
| New               | `q"new $init"`                                                           |
| New Anonymous     | `q"new { ..$stat } with ..$inits { $self => ..$stats }"`                 |
| Placeholder       | `q"_"`                                                                   |
| Eta Expansion     | `q"$expr _"`                                                             |
| Repeated          | `q"$expr: _*"`                                                           |

## Types (meta.Type)

|                   | Quasiquote                                                            |
| ----------------- | --------------------------------------------------------------------- |
| Literal           | `t"<literal>"`                                                        |
| Name              | `t"<name>"`                                                           |
| Selection         | `t"$eref.$tname"`                                                     |
| Projection        | `t"$tpe#$tname"`                                                      |
| Singleton         | `t"$eref.type"`                                                       |
| Application       | `t"$tpe[..$tpesnel]` (vote for #519 to support `q"$expr[...$tpess]"`) |
| Infix Application | `t"$tpe $tname $tpe"`                                                 |
| With              | `t"$tpe with $tpe"` (only for supported dialects)                     |
| And               | `t"$tpe & $tpe"` (only for supported dialects)                        |
| Or                | <code>t"$tpe &#124; $tpe"</code> (only for supported dialects)        |
| Function          | `t"(..$tpes) => $tpe"`                                                |
| Implicit Function | `t"implicit (..$tpes) => $tpe"`                                       |
| Tuple             | `t"(..$tpesnel)"`                                                     |
| Refine            | `t"$tpeopt { ..$stats }"`                                             |
| Existential       | `t"$tpe forSome { ..$statsnel }"`                                     |
| Annotate          | `t"$tpe ..@$annotsnel"`                                               |
| Lambda            | `t[..$tparams] => $tpe`                                               |
| Method            | `t(...$paramss): $tpe`                                                |
| Placeholder       | `t"_ >: $tpeopt <: $tpeopt"`                                          |
| By Name           | `t"=> $tpe"`                                                          |
| Repeated          | `t"$tpe*"`                                                            |
| Var               | Not supported                                                         |

## Patterns (meta.Pat) and Cases (meta.Case)

|                   | Quasiquote                          |
| ----------------- | ----------------------------------- |
| Literal           | `p"<lit>"`                          |
| Wildcard          | `p"_"`                              |
| Sequence Wildcard | `p"_*"`                             |
| Var               | `p"<name>"`                         |
| Bind              | `p"$pat @ $pat"`                    |
| Alternative       | <code>p"$pat &#124; $pat"</code>    |
| Tuple             | `p"(..$patsnel)"`                   |
| Extract           | `p"$expr(..$pats)"`                 |
| Infix Extract     | `p"$pat $ename (..$pats)"`          |
| Interpolation     | Not supported yet                   |
| Typed             | `p"$pat: $tpe"`                     |
| Name              | `` p"`<name>`" ``                   |
| Selection         | `p"$expr.$ename"`                   |
| Case              | `p"case $pat if $expropt => $expr"` |

## Members (meta.Member)

### Declarations

|      | Quasiquote                                                 |
| ---- | ---------------------------------------------------------- |
| Val  | `q"..$mods val ..$patsnel: $tpe"`                          |
| Var  | `q"..$mods var ..$patsnel: $tpe"`                          |
| Def  | `q"..$mods def $ename[..$tparams](...$paramss): $tpe"`     |
| Type | `q"..$mods type $tname[..$tparams] >: $tpeopt <: $tpeopt"` |

### Definitions

|                | Quasiquote                                                                        |
| -------------- | --------------------------------------------------------------------------------- |
| Val            | `q"..$mods val ..$patsnel: $tpeopt = $expr"`                                      |
| Var            | `q"..$mods var ..$patsnel: $tpeopt = $expropt"`                                   |
| Def            | `q"..$mods def $ename[..$tparams](...$paramss): $tpeopt = $expr"`                 |
| Macro          | `q"..$mods def $ename[..$tparams](...$paramss): $tpeopt = macro $expr"`           |
| Type           | `q"..$mods type $tname[..$tparams] = $tpe"`                                       |
| Class          | `q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) extends $template"` |
| Trait          | `q"..$mods trait $tname[..$tparams] extends $template"`                           |
| Object         | `q"..$mods object $ename extends $template"`                                      |
| Package Object | `q"package object $ename extends $template"`                                      |
| Package        | `q"package $eref { ..$stats }"`                                                   |
| Primary Ctor   | `q"..$mods def this(...$paramss)"`                                                |
| Secondary Ctor | `q"..$mods def this(...$paramss) = $expr"`                                        |

### Value Parameters (meta.Term.Param)

|            | Quasiquote                                 |
| ---------- | ------------------------------------------ |
| Term Param | `param"..$mods $name: $tpeopt = $expropt"` |

### Type Parameters (meta.Type.Param)

|            | Quasiquote                                                                     |
| ---------- | ------------------------------------------------------------------------------ |
| Type Param | `tparam"..$mods $name[..$tparams] >: $tpeopt <: $tpeopt <% ..$tpes : ..$tpes"` |

## Inits (meta.Init)

|      | Quasiquote                                         |
| ---- | -------------------------------------------------- |
| Init | `init"$tpe(...$exprss)"`, `init"this(...$exprss)"` |

## Selfs (meta.Self)

|      | Quasiquote                                    |
| ---- | --------------------------------------------- |
| Self | `self"$name: $tpeopt"`, `self"this: $tpeopt"` |

## Template (meta.Template)

|          | Quasiquote                                                                                                                                                            |
| -------- | --------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| Template | `template"{ ..$stats } with ..$inits { $self => ..$stats }"` (first `stats` is early initializers, second `stats` is regular statements in the body of the template). |

## Modifiers (meta.Mod)

|               | Quasiquote             |
| ------------- | ---------------------- |
| Annotation    | `mod"@$annot"`         |
| Private       | `mod"private[$ref]"`   |
| Protected     | `mod"protected[$ref]"` |
| Implicit      | `mod"implicit"`        |
| Final         | `mod"final"`           |
| Sealed        | `mod"sealed"`          |
| Override      | `mod"override"`        |
| Case          | `mod"case"`            |
| Abstract      | `mod"abstract"`        |
| Covariant     | `mod"+"`               |
| Contravariant | `mod"-"`               |
| Lazy          | `mod"lazy"`            |
| Val Param     | `mod"valparam"`        |
| Var Param     | `mod"varparam"`        |
| Inline        | `mod"inline"`          |

## Enumerators (meta.Enum)

|           | Quasiquote                  |
| --------- | --------------------------- |
| Generator | `enumerator"$pat <- $expr"` |
| Value     | `enumerator"$pat = $expr"`  |
| Guard     | `enumerator"if $expr"`      |

## Imports (meta.Import)

|        | Quasiquote                  |
| ------ | --------------------------- |
| Import | `q"import ..$importersnel"` |

## Importer (meta.Importer)

|          | Quasiquote                          |
| -------- | ----------------------------------- |
| Importer | `importer"$eref.{..$importeesnel}"` |

## Importees (meta.Importee)

|          | Quasiquote                 |
| -------- | -------------------------- |
| Name     | `importee"$name"`          |
| Rename   | `importee"$name => $name"` |
| Unimport | `importee"$name => _"`     |
| Wildcard | `importee"_"`              |

## Sources (meta.Source)

|        | Quasiquote         |
| ------ | ------------------ |
| Source | `source"..$stats"` |

## Legend

The tables above define quasiquote syntax using a notation called _quasiquote
templates_. A quasiquote is valid if it conforms to exactly one quasiquote
template according to the following rules:

1. Any trivia token (e.g. whitespace and comments) in a quasiquote template or a
   quasiquote is insignificant and is ignored for the purposes of conformance
   testing.
1. Any non-trivia token in a quasiquote template, except for an unquote
   template, means that exactly that token is required in a quasiquote, with the
   following exceptions:

   1. Parentheses, brackets and braces around unquotes are oftentimes dropped if
      they wrap empty lists, e.g. `q"x + y"` conforms to
      `q"$expr $ename[..$tpes] $expr"`.
   1. `with` is dropped if there are zero or one inits, e.g. both `q"new {}"`
      and `q"new C"` conform to
      `q"new { ..$stat } with ..$inits { $self => ..$stats }`.
   1. This list is probably incomplete. Please
      [submit an issue](https://github.com/scalameta/scalameta/issues/new) if
      you find any discrepancies.

1. An _unquote template_ (`$smth`, `..$smth` or `...$smth`) works as follows:

   1. First, we strip standard suffixes from `smth` using the "Suffixes" table
      (e.g. `exprssnel` means a non-empty list of lists of `expr`).
   1. Second, we figure out the expected type of `smth` using the "Shorthands"
      table (e.g. `expr` means `Term`, so `exprssnel` means `List[List[Term]]`).
   1. Third, we apply an appropriate number of replications to the unquote
      template to have it match the corresponding part of a quasiquote that's
      being tested for conformance:

      1. `$smth` can not be replicated.
      1. `..$smth` means an arbitrary mix of `$smth` and `..$smth` unquote
         templates separated according to their location (e.g. an empty string,
         `[$tpe]`, `[..$tpes, $tpe]` all conform to `[..$tpes]`, and the
         separator is a comma, as appropriate for a list of type arguments).
      1. `...$smth` means an arbitrary mix of `$smth`, `..$smth` and `...$smth`
         unquote templates, separated according to their location (e.g. an empty
         string, `(...$exprss)`, `(..$exprs)($expr1, $expr2)()` all conform to
         `(...$exprss)`, and the separator are matching parentheses, as
         appropriate for a list of arguments).
      1. If a suffix of `smth` says that it's a non-empty list, then replication
         can't result in an empty list.
      1. If a quasiquote is used as a pattern, then some replications may be
         illegal.

   1. Finally, we match the unquotes after replication against the corresponding
      parts of the quasiquote under conformance test. There are three
      possibilities for a match: scala syntax, unquote, lifted unquote.

1. If not specified explicitly, quasiquote templates work for both construction
   and deconstruction. In some cases, a template is only applicable to
   construction (e.g. it's impossible to pattern match a name without specifying
   an expected type explicitly, because patterns like in
   `term match { case q"$ename" => }` will match any term, not limited to just
   term names).

### Shorthands

| Type            | Shorthand     |
| --------------- | ------------- |
| meta.Case       | `$case`       |
| meta.Enumerator | `$enumerator` |
| meta.Mod        | `$mod`        |
| meta.Mod.Annot  | `$annot`      |
| meta.Name       | `$name`       |
| meta.Importee   | `$importee`   |
| meta.Importer   | `$importer`   |
| meta.Init       | `$init`       |
| meta.Pat        | `$pat`        |
| meta.Ref        | `$ref`        |
| meta.Self       | `$self`       |
| meta.Stat       | `$stat`       |
| meta.Template   | `$template`   |
| meta.Term       | `$expr`       |
| meta.Term.Name  | `$ename`      |
| meta.Term.Ref   | `$eref`       |
| meta.Term.Param | `$param`      |
| meta.Type       | `$tpe`        |
| meta.Type.Name  | `$tname`      |
| meta.Type.Param | `$tparam`     |

### Suffixes

| Suffix | Wrapped Type    | Example                         |
| ------ | --------------- | ------------------------------- |
| -s     | `List[_]`       | `exprs: List[meta.Term]`        |
| -ss    | `List[List[_]]` | `exprss: List[List[meta.Term]]` |
| -opt   | `Option[_]`     | `expropt: Option[meta.Term]`    |
| -nel   | `_`             | `tpesnel: List[meta.Type]`      |
