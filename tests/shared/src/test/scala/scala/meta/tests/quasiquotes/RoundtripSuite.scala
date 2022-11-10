package scala.meta.tests
package quasiquotes

import scala.meta._
import scala.meta.dialects.Scala211

object RoundtripSuite {
  val tree: Tree = ???
  tree match {
    case q"$name.this" => q"$name.this"
    case q"$name.super" => q"$name.super"
    case q"super[$name]" => q"super[$name]"
    case q"$name1.super[$name2]" => q"$name1.super[$name2]"
    case q"${name: Term.Name}" => q"${name: Term.Name}"
    case q"$expr.$name" => q"$expr.$name"
    case q"$expr(...$exprssnel)" => q"$expr(...$exprssnel)"
    case q"$expr(..$exprssnel)" => q"$expr(..$exprssnel)"
    case q"$expr[..$tpesnel]" => q"$expr[..$tpesnel]"
    case q"$expr1 $name[..$tpes] $expr2" => q"$expr1 $name[..$tpes] $expr2"
    case q"$expr $name[..$tpes] (..$exprs)" => q"$expr $name[..$tpes] (..$exprs)"
    case q"!$expr" => q"!$expr"
    case q"~$expr" => q"~$expr"
    case q"-$expr" => q"-$expr"
    case q"+$expr" => q"+$expr"
    case q"$ref = $expr" => q"$ref = $expr"
    case q"$expr1(...$exprssnel) = $expr2" => q"$expr1(...$exprssnel) = $expr2"
    case q"$expr1(..$exprssnel) = $expr2" => q"$expr1(..$exprssnel) = $expr2"
    case q"return $expr" => q"return $expr"
    case q"throw $expr" => q"throw $expr"
    case q"$expr: $tpe" => q"$expr: $tpe"
    case q"$expr: ..@$annotsnel" => q"$expr: ..@$annotsnel"
    case q"(..$exprsnel)" => q"(..$exprsnel)"
    case q"{ ..$stats }" => q"{ ..$stats }"
    case q"if ($expr1) $expr2 else $expr3" => q"if ($expr1) $expr2 else $expr3"
    case q"$expr match { ..case $casesnel }" => q"$expr match { ..case $casesnel }"
    case q"try $expr catch { ..case $cases } finally $expropt" =>
      q"try $expr catch { ..case $cases } finally $expropt"
    case q"try $expr1 catch $expr2 finally $expropt" => q"try $expr1 catch $expr2 finally $expropt"
    case q"(..$params) => $expr" => q"(..$params) => $expr"
    case q"{ ..case $casesnel }" => q"{ ..case $casesnel }"
    case q"while ($expr1) $expr2" => q"while ($expr1) $expr2"
    case q"do $expr1 while($expr2)" => q"do $expr1 while($expr2)"
    case q"for (..$enumeratorsnel) $expr" => q"for (..$enumeratorsnel) $expr"
    case q"for (..$enumeratorsnel) yield $expr" => q"for (..$enumeratorsnel) yield $expr"
    case q"new { ..$stat } with ..$ctorcalls { $param => ..$stats }" =>
      q"new { ..$stat } with ..$ctorcalls { $param => ..$stats }"
    case q"$expr _" => q"$expr _"
    case q"$expr: _*" => q"$expr: _*"
    case q"${lit: Lit}" => q"${lit: Lit}"
    case t"${name: Type.Name}" => t"${name: Type.Name}"
    case t"$ref.$tname" => t"$ref.$tname"
    case t"$tpe#$tname" => t"$tpe#$tname"
    case t"$ref.type" => t"$ref.type"
    case t"$tpe[..$tpesnel]" => t"$tpe[..$tpesnel]"
    case t"$tpe1 $tname $tpe2" => t"$tpe1 $tname $tpe2"
    case t"$tpe1 with $tpe2" => t"$tpe1 with $tpe2"
    case t"$tpe1 & $tpe2" => t"$tpe1 & $tpe2"
    case t"$tpe1 | $tpe2" => t"$tpe1 | $tpe2"
    case t"(..$tpes) => $tpe" => t"(..$tpes) => $tpe"
    case t"(..$tpesnel)" => t"(..$tpesnel)"
    case t"$tpeopt { ..$stats }" => t"$tpeopt { ..$stats }"
    case t"$tpe forSome { ..$statsnel }" => t"$tpe forSome { ..$statsnel }"
    case t"$tpe ..@$annotsnel" => t"$tpe ..@$annotsnel"
    case t"_ >: $tpeopt1 <: $tpeopt2" => t"_ >: $tpeopt1 <: $tpeopt2"
    case t"=> $tpe" => t"=> $tpe"
    case t"$tpe*" => t"$tpe*"
    case p"$pat1 @ $pat2" => p"$pat1 @ $pat2"
    case p"$pat1 | $pat2" => p"$pat1 | $pat2"
    case p"(..$patsnel)" => p"(..$patsnel)"
    case p"$ref[..$tpes](..$pats)" => p"$ref[..$tpes](..$pats)"
    case p"$pat $name (..$patsnel)" => p"$pat $name (..$patsnel)"
    case p"$pat: $ptpe" => p"$pat: $ptpe"
    case p"$expr.$name" => p"$expr.$name"
    case p"case $pat if $expropt => $expr" => p"case $pat if $expropt => $expr"
    case q"import ..$importersnel" => q"import ..$importersnel"
    case q"..$mods val ..$patsnel: $tpe" => q"..$mods val ..$patsnel: $tpe"
    case q"..$mods var ..$patsnel: $tpe" => q"..$mods var ..$patsnel: $tpe"
    case q"..$mods def $name[..$tparams](...$paramss): $tpe" =>
      q"..$mods def $name[..$tparams](...$paramss): $tpe"
    case q"..$mods type $tname[..$tparams] >: $tpeopt1 <: $tpeopt2" =>
      q"..$mods type $tname[..$tparams] >: $tpeopt1 <: $tpeopt2"
    case q"..$mods val ..$patsnel: $tpeopt = $expr" => q"..$mods val ..$patsnel: $tpeopt = $expr"
    case q"..$mods var ..$patsnel: $tpeopt = $expropt" =>
      q"..$mods var ..$patsnel: $tpeopt = $expropt"
    case q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" =>
      q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr"
    case q"..$mods def $name[..$tparams](...$paramss): $tpeopt = macro $expr" =>
      q"..$mods def $name[..$tparams](...$paramss): $tpeopt = macro $expr"
    case q"..$mods type $tname[..$tparams] = $tpe" => q"..$mods type $tname[..$tparams] = $tpe"
    case q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) $template" =>
      q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) $template"
    case q"..$mods trait $tname[..$tparams] $template" =>
      q"..$mods trait $tname[..$tparams] $template"
    case q"..$mods object $name $template" => q"..$mods object $name $template"
    case q"package object $name $template" => q"package object $name $template"
    case q"package $ref { ..$stats }" => q"package $ref { ..$stats }"
    case q"..$mods def this(...$paramss)" => q"..$mods def this(...$paramss)"
    case q"..$mods def this(...$paramss) = $expr" => q"..$mods def this(...$paramss) = $expr"
    case param"..$mods $paramname: $tpeopt = $expropt" =>
      param"..$mods $paramname: $tpeopt = $expropt"
    case tparam"..$mods $tparamname[..$tparams] >: $tpeopt1 <: $tpeopt2 <% ..$tpes1 : ..$tpes2" =>
      tparam"..$mods $tparamname[..$tparams] >: $tpeopt1 <: $tpeopt2 <% ..$tpes1 : ..$tpes2"
    case init"$tpe(...$exprss)" => init"$tpe(...$exprss)"
    case init"$tpe(..$exprss)" => init"$tpe(..$exprss)"
    case template"{ ..$stats1 } with ..$ctorcalls { $param => ..$stats2 }" =>
      template"{ ..$stats1 } with ..$ctorcalls { $param => ..$stats2 }"
    case mod"@$annot" => mod"@$annot"
    case mod"private[$name]" => mod"private[$name]"
    case mod"protected[$name]" => mod"protected[$name]"
    case enumerator"$pat <- $expr" => enumerator"$pat <- $expr"
    case enumerator"$pat = $expr" => enumerator"$pat = $expr"
    case enumerator"if $expr" => enumerator"if $expr"
    case importer"$ref.{..$importeesnel}" => importer"$ref.{..$importeesnel}"
    case importee"${iname: Name.Indeterminate}" => importee"${iname: Name.Indeterminate}"
    case importee"$iname1 => $iname2" => importee"$iname1 => $iname2"
    case importee"$iname => _" => importee"$iname => _"
    case source"..$stats" => source"..$stats"
  }
}
