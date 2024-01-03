package scala.meta.tests
package quasiquotes

import scala.meta._
import scala.meta.dialects.Scala211
import scala.meta.quasiquotes._

object RoundtripSuite {
  val tree: Tree = ???
  tree match {
    case q"${name: Name}.this" => q"$name.this"
    case q"${name: Name}.super" => q"name.super"
    case q"super[${name: Name}]" => q"super[$name]"
    case q"${name1: Name}.super[${name2: Name}]" => q"$name1.super[$name2]"
    case q"${name: Term.Name}" => q"${name: Term.Name}"
    case q"${expr: Term}.${name: Term.Name}" => q"$expr.$name"
    case q"${expr: Term}(...${exprssnel: List[Term.ArgClause]})" => q"$expr(...$exprssnel)"
    case q"${expr: Term}(..${exprssnel: Term.ArgClause})" => q"$expr(..$exprssnel)"
    case q"${expr: Term}[..${tpesnel: Type.ArgClause}]" => q"$expr[..$tpesnel]"
    case q"${expr1: Term} ${name: Term.Name}[..${tpes: Type.ArgClause}] ${expr2: Term.Name}" => q"$expr1 $name[..$tpes] $expr2"
    case q"${expr: Term.Name} ${name: Term.Name}[..${tpes: Type.ArgClause}] (..${exprs: Term.ArgClause})" => q"$expr $name[..$tpes] (..$exprs)"
    case q"!${expr: Term}" => q"!$expr"
    case q"~${expr: Term}" => q"~$expr"
    case q"-${expr: Term}" => q"-$expr"
    case q"+${expr: Term}" => q"+$expr"
    case q"${ref: Term} = ${expr: Term}" => q"$ref = $expr"
    case q"${expr1: Term}(...${exprssnel: List[Term.ArgClause]}) = ${expr2: Term}" => q"$expr1(...$exprssnel) = $expr2"
    case q"${expr1: Term}(..${exprssnel: Term.ArgClause}) = ${expr2: Term}" => q"$expr1(..$exprssnel) = $expr2"
    case q"return ${expr: Term}" => q"return $expr"
    case q"throw ${expr: Term}" => q"throw $expr"
    case q"${expr: Term}: ${tpe: Type}" => q"$expr: $tpe"
    case q"${expr: Term}: ..@${annotsnel: List[Mod.Annot]}" => q"$expr: ..@$annotsnel"
    case q"(..${exprsnel: List[Term]})" => q"(..$exprsnel)"
    case q"{ ..${stats: List[Stat]} }" => q"{ ..${stats: List[Stat]} }"
    case q"if (${expr1: Term}) ${expr2: Term} else ${expr3: Term}" => q"if ($expr1) $expr2 else $expr3"
    case q"${expr: Term} match { ..case ${casesnel: List[Case]} }" => q"$expr match { ..case $casesnel }"
    case q"try ${expr: Term} catch { ..case ${cases: List[Case]} } finally ${expropt: Option[Term]}" =>
      q"try $expr catch { ..case $cases } finally $expropt"
    case q"try ${expr1: Term} catch ${expr2: Term} finally ${expropt: Option[Term]}" => q"try $expr1 catch $expr2 finally $expropt"
    case q"(..${params: Term.ParamClause}) => ${expr: Term}" => q"(..$params) => $expr"
    case q"{ ..case ${casesnel: List[Case]} }" => q"{ ..case $casesnel }"
    case q"while (${expr1: Term}) ${expr2: Term}" => q"while ($expr1) $expr2"
    case q"do ${expr1: Term} while(${expr2: Term})" => q"do $expr1 while($expr2)"
    case q"for (..${enumeratorsnel: List[Enumerator]}) ${expr: Term}" => q"for (..$enumeratorsnel) $expr"
    case q"for (..${enumeratorsnel: List[Enumerator]}) yield ${expr: Term}" => q"for (..$enumeratorsnel) yield $expr"
    case q"new { ..${stat: List[Stat]} } with ..${ctorcalls: List[Init]} { ${param: Self} => ..${stats: List[Stat]} }" =>
      q"new { ..$stat } with ..$ctorcalls { $param => ..$stats }"
    case q"${expr: Term} _" => q"$expr _"
    case q"${expr: Term}: _*" => q"$expr: _*"
    case q"${lit: Lit}" => q"${lit: Lit}"
    case t"${name: Type.Name}" => t"${name: Type.Name}"
    case t"${ref: Term.Ref}.${tname: Type.Name}" => t"$ref.$tname"
    case t"${tpe: Type}#${tname: Type.Name}" => t"$tpe#$tname"
    case t"${ref: Term.Ref}.type" => t"$ref.type"
    case t"${tpe: Type}[..${tpesnel: Type.ArgClause}]" => t"$tpe[..$tpesnel]"
    case t"${tpe1: Type} ${tname: Type.Name} ${tpe2: Type}" => t"$tpe1 $tname $tpe2"
    case t"${tpe1: Type} with ${tpe2: Type}" => t"$tpe1 with $tpe2"
    case t"${tpe1: Type} & ${tpe2: Type}" => t"$tpe1 & $tpe2"
    case t"${tpe1: Type} | ${tpe2: Type}" => t"$tpe1 | $tpe2"
    case t"(..${tpes: Type.FuncParamClause}) => ${tpe: Type}" => t"(..$tpes) => $tpe"
    case t"(..${tpesnel: List[Type]})" => t"(..$tpesnel)"
    case t"${tpeopt: Option[Type]} { ..${stats: List[Stat]} }" => t"$tpeopt { ..$stats }"
    case t"${tpe: Type} forSome { ..${statsnel: List[Stat]} }" => t"$tpe forSome { ..$statsnel }"
    case t"${tpe: Type} ..@${annotsnel: List[Mod.Annot]}" => t"$tpe ..@$annotsnel"
    case t"_ >: ${tpeopt1: Option[Type]} <: ${tpeopt2: Option[Type]}" => t"_ >: $tpeopt1 <: $tpeopt2"
    case t"=> ${tpe: Type}" => t"=> $tpe"
    case t"${tpe: Type}*" => t"$tpe*"
    case p"${pat1: Pat} @ ${pat2: Pat}" => p"$pat1 @ $pat2"
    case p"${pat1: Pat} | ${pat2: Pat}" => p"$pat1 | $pat2"
    case p"(..${patsnel: List[Pat]})" => p"(..$patsnel)"
    case p"${ref: Term}[..${tpes: Type.ArgClause}](..${pats: Pat.ArgClause})" => p"$ref[..$tpes](..$pats)"
    case p"${pat: Pat} ${name: Term.Name} (..${patsnel: Pat.ArgClause})" => p"$pat $name (..$patsnel)"
    case p"${pat: Pat}: ${ptpe: Type}" => p"$pat: $ptpe"
    case p"${expr: Term}.${name: Term.Name}" => p"$expr.$name"
    case p"case ${pat: Pat} if ${expropt: Option[Term]} => ${expr: Term}" => p"case $pat if $expropt => $expr"
    case q"import ..${importersnel: List[Importer]}" => q"import ..$importersnel"
    case q"..${mods: List[Mod]} val ..${patsnel: List[Pat]}: ${tpe: Type}" => q"..$mods val ..$patsnel: $tpe"
    case q"..${mods: List[Mod]} var ..${patsnel: List[Pat]}: ${tpe: Type}" => q"..$mods var ..$patsnel: $tpe"
    case q"..${mods: List[Mod]} def ${name: Term.Name}[..${tparams: Type.ParamClause}](...${paramss: List[Term.ParamClause]}): ${tpe: Type}" =>
      q"..$mods def $name[..$tparams](...$paramss): $tpe"
    case q"..${mods: List[Mod]} type ${tname: Type.Name}[..${tparams: Type.ParamClause}] >: ${tpeopt1: Option[Type]} <: ${tpeopt2: Option[Type]}" =>
      q"..$mods type $tname[..$tparams] >: $tpeopt1 <: $tpeopt2"
    case q"..${mods: List[Mod]} val ..${patsnel: List[Pat]}: ${tpeopt: Type} = ${expr: Term}" => q"..$mods val ..$patsnel: $tpeopt = $expr"
    case q"..${mods: List[Mod]} var ..${patsnel: List[Pat]}: ${tpeopt: Type} = ${expropt: Term}" =>
      q"..$mods var ..$patsnel: $tpeopt = $expropt"
    case q"..${mods: List[Mod]} def ${name: Term.Name}[..${tparams: Type.ParamClause}](...${paramss: List[Term.ParamClause]}): ${tpeopt: Option[Type]} = ${expr: Term}" =>
      q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr"
    case q"..${mods: List[Mod]} def ${name: Term.Name}[..${tparams: Type.ParamClause}](...${paramss: List[Term.ParamClause]}): ${tpeopt: Option[Type]} = macro ${expr: Term}" =>
      q"..$mods def $name[..$tparams](...$paramss): $tpeopt = macro $expr"
    case q"..${mods: List[Mod]} type ${tname: Type.Name}[..${tparams: Type.ParamClause}] = ${tpe: Type}" =>
      q"..$mods type $tname[..$tparams] = $tpe"
    case q"..${mods: List[Mod]} class ${tname: Type.Name}[..${tparams: Type.ParamClause}] ..${ctorMods: List[Mod]} (...${paramss: List[Term.ParamClause]}) ${template: Template}" =>
      q"..$mods class $tname[..$tparams] ..$ctorMods (...$paramss) $template"
    case q"..${mods: List[Mod]} trait ${tname: Type.Name}[..${tparams: Type.ParamClause}] ${template: Template}" =>
      q"..$mods trait $tname[..$tparams] $template"
    case q"..${mods: List[Mod]} object ${name: Term.Name} ${template: Template}" => q"..$mods object $name $template"
    case q"package object ${name: Term.Name} ${template: Template}" => q"package object $name $template"
    case q"package ${ref: Term.Ref} { ..${stats: List[Stat]} }" => q"package $ref { ..$stats }"
    case q"..${mods: List[Mod]} def this(...${paramss: List[Term.ParamClause]})" => q"..$mods def this(...$paramss)"
    case q"..${mods: List[Mod]} def this(...${paramss: List[Term.ParamClause]}) = ${expr: Init}" => q"..$mods def this(...$paramss) = $expr"
    case param"..${mods: List[Mod]} ${paramname: Name}: ${tpeopt: Option[Type]} = ${expropt: Option[Term]}" =>
      param"..$mods $paramname: $tpeopt = $expropt"
    case tparam"..${mods: List[Mod]} ${tparamname: Name}[..${tparams: Type.ParamClause}] >: ${tpeopt1: Option[Type]} <: ${tpeopt2: Type} <% ..${tpes1: List[Type]} : ..${tpes2: List[Type]}" =>
      tparam"..$mods $tparamname[..$tparams] >: $tpeopt1 <: $tpeopt2 <% ..$tpes1 : ..$tpes2"
    case init"${tpe: Type}(...${exprss: List[Term.ArgClause]})" => init"$tpe(...$exprss)"
    case init"${tpe: Type}(..${exprss: Term.ArgClause})" => init"$tpe(..$exprss)"
    case template"{ ..${stats1: List[Stat]} } with ..${ctorcalls: List[Init]} { ${param: Self} => ..${stats2: List[Stat]} }" =>
      template"{ ..$stats1 } with ..$ctorcalls { $param => ..$stats2 }"
    case mod"@${annot: Mod.Annot}" => mod"@$annot"
    case mod"private[${name: Ref}]" => mod"private[$name]"
    case mod"protected[${name: Ref}]" => mod"protected[$name]"
    case enumerator"${pat: Pat} <- ${expr: Term}" => enumerator"$pat <- $expr"
    case enumerator"${pat: Pat} = ${expr: Term}" => enumerator"$pat = $expr"
    case enumerator"if ${expr: Term}" => enumerator"if $expr"
    case importer"${ref: Term.Ref}.{..${importeesnel: List[Importee]}}" => importer"$ref.{..$importeesnel}"
    case importee"${iname: Name.Indeterminate}" => importee"${iname: Name.Indeterminate}"
    case importee"${iname1: Name} => ${iname2: Name}" => importee"$iname1 => $iname2"
    case importee"${iname: Name} => _" => importee"$iname => _"
    case source"..${stats: List[Stat]}" => source"..$stats"
  }
}
