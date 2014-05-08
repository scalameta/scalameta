package scala.reflect.parser
import scala.reflect.core._, Aux._
import org.scalareflect.Print
import org.scalareflect.PrintUtil.{ seq => p, rep => r }
import scala.reflect.parser.ParserInfo._

object NaivePrinters {
  // Term
  implicit val printTermIdent: Print[Term.Ident] = Print { t => printIdent(t) }
  implicit val printTermSelect: Print[Term.Select] = Print { t =>
    p(t.qual, ".", t.selector)
  }
  implicit val printTermSuperSelect: Print[Term.SuperSelect] = Print { t =>
    p(t.qual.map { qual => p(qual, ".") }.getOrElse(""),
      "super", t.supertpe.map { st => p("[", st, "]") }.getOrElse(""),
      ".", t.selector)
  }
  implicit val printTermThis: Print[Term.This] = Print { t =>
    p(t.qual.map { qual => p(qual, ".") }.getOrElse(""), "this")
  }
  implicit val printTermInterpolate: Print[Term.Interpolate] = Print { t =>
    val zipped = t.parts.zip(t.args).map {
      case (part, id: Ident) if !id.isBackquoted => p(part, "$", id.value)
      case (part, arg)                           => p(part, "${", arg, "}")
    }
    p(t.prefix, "\"", r(zipped, ""), t.parts.last, "\"")
  }
  implicit val printTermApply: Print[Term.Apply] = Print { t =>
    p(t.fun, t.args)
  }
  implicit val printTermApplyRight: Print[Term.ApplyRight] = Print { app =>
    p(app.lhs, " ", app.op, app.targs, " ", app.rhs)
  }
  implicit val printTermApplyType: Print[Term.ApplyType] = Print { app =>
    p(app.fun, app.targs)
  }
  implicit val printTermApplyUnary: Print[Term.ApplyUnary] = Print { app =>
    p(app.op, app.arg)
  }
  implicit val printTermAssign: Print[Term.Assign] = Print { assign =>
    p(assign.lhs, " = ", assign.rhs)
  }
  implicit val printTermUpdate: Print[Term.Update] = Print { upd =>
    p(upd.lhs, " = ", upd.rhs)
  }
  implicit val printTermReturn: Print[Term.Return] = Print { ret =>
    p("return ", ret.expr)
  }
  implicit val printTermThrow: Print[Term.Throw] = Print { thr =>
    p("throw ", thr.expr)
  }
  implicit val printTermAscribe: Print[Term.Ascribe] = Print { ascr =>
    p(ascr.expr, ": ", ascr.ascribedTpe)
  }
  implicit val printTermAnnotate: Print[Term.Annotate] = Print { ann =>
    p(ann.expr, ": ", ann.mods)
  }
  implicit val printTermTuple: Print[Term.Tuple] = Print { tup =>
    p("(", r(tup.elements, ", "), ")")
  }
  implicit val printTermBlock: Print[Term.Block] = Print { block =>
    p("{ ", r(block.stats, "; "), " }")
  }
  implicit val printTermIf: Print[Term.If] = Print { tif =>
    p("if (", tif.cond, ") ", tif.thenp, tif.elsep.map { t => p(" then ", t) }.getOrElse(""))
  }
  implicit val printTermMatch: Print[Term.Match] = Print { tmatch =>
    p(tmatch.scrut, " match ", tmatch.cases)
  }
  implicit val printTermTry: Print[Term.Try] = Print { ttry =>
    p("try ", ttry.expr,
      ttry.catchp.map { t => p(" catch ", t) }.getOrElse(""),
      ttry.finallyp.map { t => p(" finally ", t) }.getOrElse(""))
  }
  implicit val printTermFunction: Print[Term.Function] = Print {
    case Term.Function(Param(mods, Some(name), tptopt, _) :: Nil, body) if mods.has[Mod.Implicit] =>
      p("{ implicit ", name, tptopt.map { tpt => p(": ", tpt) }.getOrElse(""), body)
    case Term.Function(Param(_, None, None, _) :: Nil, body) =>
      p("_ => ", body)
    case Term.Function(params, body) =>
      p("(", r(params, ","), ") => ", body)
  }
  implicit val printTermCases: Print[Term.Cases] = Print { cases =>
    p("{ ", r(cases.cases, "; "), " }")
  }
  implicit val printTermWhile: Print[Term.While] = Print { twhile =>
    p("while (", twhile.expr, ") ", twhile.body)
  }
  implicit val printTermDo: Print[Term.Do] = Print { tdo =>
    p("do ", tdo.body, " while (", tdo.expr, ")")
  }
  implicit val printTermFor: Print[Term.For] = Print { tfor =>
    p("for (", r(tfor.enums, "; "), ") ", tfor.body)
  }
  implicit val printTermForYield: Print[Term.ForYield] = Print { tfor =>
    p("for (", r(tfor.enums, "; "), ") yield ", tfor.body)
  }
  implicit val printTermNew: Print[Term.New] = Print { tnew =>
    p("new", tnew.templ)
  }
  implicit val printTermPlaceholder: Print[Term.Placeholder] = Print { pl =>
    p("_")
  }
  implicit val printTermEta: Print[Term.Eta] = Print { eta =>
    p(eta.term, " _")
  }

  // Lit
  implicit val printLitTrue: Print[Lit.True]     = Print { _ => "true" }
  implicit val printLitFalse: Print[Lit.False]   = Print { _ => "true" }
  implicit val printLitInt: Print[Lit.Int]       = Print { t => t.value.toString }
  implicit val printLitLong: Print[Lit.Long]     = Print { t => t.value.toString }
  implicit val printLitFloat: Print[Lit.Float]   = Print { t => t.value.toString }
  implicit val printLitDouble: Print[Lit.Double] = Print { t => t.value.toString }
  implicit val printLitChar: Print[Lit.Char]     = Print { t => t.value.toString }
  implicit val printLitString: Print[Lit.String] = Print { t => t.value }
  implicit val printLitSymbol: Print[Lit.Symbol] = Print { t => p("'", t.value.name) }
  implicit val printLitNull: Print[Lit.Null]     = Print { _ => "null" }
  implicit val printLitUnit: Print[Lit.Unit]     = Print { _ => "()" }

  // Type and ParamType
  implicit val printParamTypeByName: Print[ParamType.ByName] = Print { t =>
    p("=> ", t.tpe)
  }
  implicit val printParamTypeRepeated: Print[ParamType.Repeated] = Print { t =>
    p(t.tpe, "*")
  }
  implicit val printTypeIdent: Print[Type.Ident] = Print { t => printIdent(t) }
  implicit val printTypeAnnotate: Print[Type.Annotate] = Print { t =>
    p(t.tpe, " ", t.mods)
  }
  implicit val printTypeApply: Print[Type.Apply] = Print { t =>
    p(t.tpe, "[", r(t.args, ", "), "]")
  }
  implicit val printTypeCompound: Print[Type.Compound] = Print { t =>
    p(r(t.tpes, " with "), " { ", r(t.refinement, "; "), " }")
  }
  implicit val printTypeExistential: Print[Type.Existential] = Print { t =>
    p(t.tpe, " forSome { ", r(t.quants, "; "), " }")
  }
  implicit val printTypeFunction: Print[Type.Function] = Print { t =>
    p("(", r(t.params, ", "), ") => ", t.res)
  }
  implicit val printTypePlaceholder: Print[Type.Placeholder] = Print { t =>
    p("_", t.bounds)
  }
  implicit val printTypeTuple: Print[Type.Tuple] = Print { t =>
    p("(", r(t.elements, ", "), ")")
  }

  implicit val printTypeProject: Print[Type.Project] = Print { t =>
    p(t.qual, "#", t.name)
  }
  implicit val printTypeSelect: Print[Type.Select] = Print { t =>
    p(t.qual, ".", t.name)
  }
  implicit val printTypeSingleton: Print[Type.Singleton] = Print { t =>
    p(t.ref, ".type")
  }
  implicit val printTypeSuperSelect: Print[Type.SuperSelect] = Print { t =>
    p(t.qual.map { qual => p(qual, ".") }.getOrElse(""),
      "super", t.supertpe.map { st => p("[", st, "]") }.getOrElse(""),
      ".", t.selector)
  }

  // Branches
  implicit val printTypeRef: Print[Type.Ref] = Print {
    case t: Type.Ident       => p(t)
    case t: Type.Project     => p(t)
    case t: Type.Select      => p(t)
    case t: Type.Singleton   => p(t)
    case t: Type.SuperSelect => p(t)
  }
  implicit val printParamType: Print[ParamType] = Print {
    case t: Type => p(t)
    case t: ParamType.Repeated => p(t)
    case t: ParamType.ByName => p(t)
  }
  implicit val printType: Print[Type] = Print {
    case t: Lit              => p(t)
    case t: Type.Ref         => p(t)
    case t: Type.Annotate    => p(t)
    case t: Type.Apply       => p(t)
    case t: Type.Compound    => p(t)
    case t: Type.Existential => p(t)
    case t: Type.Function    => p(t)
    case t: Type.Placeholder => p(t)
    case t: Type.Tuple       => p(t)
  }
  implicit val printLitWithType: Print[Lit with Type] = Print {
    case t: Lit.True   => p(t)
    case t: Lit.False  => p(t)
    case t: Lit.Int    => p(t)
    case t: Lit.Long   => p(t)
    case t: Lit.Float  => p(t)
    case t: Lit.Double => p(t)
    case t: Lit.Char   => p(t)
    case t: Lit.String => p(t)
    case t: Lit.Symbol => p(t)
  }
  implicit val printLit: Print[Lit] = Print {
    case t: Type       => p(t)
    case t: Lit.Null   => p(t)
    case t: Lit.Unit   => p(t)
  }
  implicit val printTermRefWithPat: Print[Term.Ref with Pat] = Print {
    case t: Term.Ident       => p(t)
    case t: Term.Select      => p(t)
  }
  implicit val printTermRef: Print[Term.Ref] = Print {
    case t: Pat              => p(t)
    case t: Term.SuperSelect => p(t)
    case t: Term.This        => p(t)
  }
  implicit val printTerm: Print[Term] = Print {
    case t: Lit              => p(t)
    case t: Term.Ref         => p(t)
    case t: Term.Interpolate => p(t)
    case t: Term.Apply       => p(t)
    case t: Term.ApplyRight  => p(t)
    case t: Term.ApplyType   => p(t)
    case t: Term.ApplyUnary  => p(t)
    case t: Term.Assign      => p(t)
    case t: Term.Update      => p(t)
    case t: Term.Return      => p(t)
    case t: Term.Throw       => p(t)
    case t: Term.Ascribe     => p(t)
    case t: Term.Annotate    => p(t)
    case t: Term.Tuple       => p(t)
    case t: Term.Block       => p(t)
    case t: Term.If          => p(t)
    case t: Term.Match       => p(t)
    case t: Term.Try         => p(t)
    case t: Term.Function    => p(t)
    case t: Term.Cases       => p(t)
    case t: Term.While       => p(t)
    case t: Term.Do          => p(t)
    case t: Term.For         => p(t)
    case t: Term.ForYield    => p(t)
    case t: Term.New         => p(t)
    case t: Term.Placeholder => p(t)
    case t: Term.Eta         => p(t)
  }
  implicit val printRef: Print[Ref] = Print {
    case t: Type.Ref => p(t)
    case t: Term.Ref => p(t)
  }
  implicit val printPat: Print[Pat] = Print {
    case t: Lit             => p(t)
    case t: Term.Ref        => p(t)
    case t: Pat.Alternative => p(t)
    case t: Pat.Bind        => p(t)
    case t: Pat.Extract     => p(t)
    case t: Pat.Tuple       => p(t)
    case t: Pat.Interpolate => p(t)
    case t: Pat.SeqWildcard => p(t)
    case t: Pat.Typed       => p(t)
    case t: Pat.Wildcard    => p(t)
  }
  implicit val printArg: Print[Arg] = Print {
    case t: Term         => p(t)
    case t: Arg.Named    => p(t)
    case t: Arg.Repeated => p(t)
  }
  implicit val printMod: Print[Mod] = Print {
    case t: Mod.Annot => p(t)
    case t: Mod.Abstract => p(t)
    case t: Mod.Case => p(t)
    case t: Mod.Contravariant => p(t)
    case t: Mod.Covariant => p(t)
    case t: Mod.Doc => p(t)
    case t: Mod.Final => p(t)
    case t: Mod.Implicit => p(t)
    case t: Mod.Lazy => p(t)
    case t: Mod.Macro => p(t)
    case t: Mod.Override => p(t)
    case t: Mod.Sealed => p(t)
    case t: Mod.Private => p(t)
    case t: Mod.Protected => p(t)
    case t: Mod.ValParam => p(t)
    case t: Mod.VarParam => p(t)
  }
  implicit val printDefn: Print[Defn] = Print {
    case t: Defn.Val       => p(t)
    case t: Defn.Var       => p(t)
    case t: Defn.Type      => p(t)
    case t: Defn.Def       => p(t)
    case t: Defn.Procedure => p(t)
    case t: Defn.Class     => p(t)
    case t: Defn.Object    => p(t)
    case t: Defn.Trait     => p(t)
  }
  implicit val printDecl: Print[Decl] = Print {
    case t: Decl.Val       => p(t)
    case t: Decl.Var       => p(t)
    case t: Decl.Type      => p(t)
    case t: Decl.Def       => p(t)
    case t: Decl.Procedure => p(t)
  }
  implicit val printPkg: Print[Pkg] = Print {
    case t: Pkg.Empty  => p(t)
    case t: Pkg.Named  => p(t)
    case t: Pkg.Object => p(t)
  }
  implicit val printCtor: Print[Ctor] = Print {
    case t: Ctor.Primary   => p(t)
    case t: Ctor.Secondary => p(t)
  }
  implicit def printEnum: Print[Enum] = Print {
    case t: Enum.Val => p(t)
    case t: Enum.Generator => p(t)
    case t: Enum.Guard => p(t)
  }

  implicit val printTree: Print[Tree] = Print {
    case t: Term => p(t)
    case t: Pat => p(t)
    case t: ParamType => p(t)
    case t: Arg => p(t)
    case t: Mod => p(t)
    case t: Defn => p(t)
    case t: Decl => p(t)
    case t: Pkg => p(t)
    case t: Ctor => p(t)
    case t: Import => p(t)
    case t: Import.Clause => p(t)
    case t: Import.Selector => p(t)
    case t: Enum => p(t)
    case t: Case => p(t)
    case t: Param => p(t)
    case t: Parent => p(t)
    case t: Self => p(t)
    case t: Template => p(t)
    case t: TypeBounds => p(t)
    case t: TypeParam => p(t)
  }

  // Imports
  implicit val printImport: Print[Import] = Print { t =>
    p("import ", r(t.clauses, ", "))
  }
  implicit val printImportClause: Print[Import.Clause] = Print { t =>
    p(t.ref, ".", t.sels)
  }
  implicit val printImportSelector: Print[Import.Selector] = Print {
    case t: Import.Selector.Rename => p(t)
    case t: Import.Selector.Name => p(t)
    case t: Import.Selector.Unimport => p(t)
    case t: Import.Selector.Wildcard => p(t)
  }
  implicit val printImportSelectorWildcard: Print[Import.Selector.Wildcard] = Print { _ => "_" }
  implicit val printImportSelectorName: Print[Import.Selector.Name] = Print { _.name }
  implicit val printImportSelectorRename: Print[Import.Selector.Rename] = Print { t => p(t.from, " => ", t.to) }
  implicit val printImportSelectorUnimport: Print[Import.Selector.Unimport] = Print { t => p(t.name, " => _") }

  // Decl
  implicit val printDeclVal: Print[Decl.Val] = Print { t =>
    p(t.mods, "val ", r(t.pats, ", "), ": ", t.decltpe )
  }
  implicit val printDeclVar: Print[Decl.Var] = Print { t =>
    p(t.mods, "var ", r(t.pats, ", "), ": ", t.decltpe )
  }
  implicit val printDeclType: Print[Decl.Type] = Print { t =>
    p(t.mods, "type ", t.name, t.tparams, t.bounds)
  }
  implicit val printDeclDef: Print[Decl.Def] = Print { t =>
    p(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits), ": ", t.decltpe)
  }
  implicit val printDeclProcedure: Print[Decl.Procedure] = Print { t =>
    p(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits))
  }

  // Defn
  implicit val printDefnVal: Print[Defn.Val] = Print { t =>
    p(t.mods, "val ", r(t.pats, ", "), t.decltpe, " = ", t.rhs)
  }
  implicit val printDefnVar: Print[Defn.Var] = Print { t =>
    p(t.mods, "var ", r(t.pats, ", "), t.decltpe, " = ", t.rhs.map(p(_)).getOrElse("_"))
  }
  implicit val printDefnType: Print[Defn.Type] = Print { t =>
    p(t.mods, "type ", t.name, t.tparams, " = ", t.body)
  }
  implicit val printDefnDef: Print[Defn.Def] = Print { t =>
    p(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits), ": ", t.decltpe, " = ", t.body)
  }
  implicit val printDefnProcedure: Print[Defn.Procedure] = Print { t =>
    p(t.mods, "def ", t.name, t.tparams, (t.explicits, t.implicits), t.body match {
      case b: Term.Block => p(b)
      case other: Term   => p("{ ", other, " }")
    })
  }
  implicit val printDefnClass: Print[Defn.Class] = Print { t =>
    p(t.mods, "class ", t.name, t.tparams, t.ctor, t.templ)
  }
  implicit val printDefnTrait: Print[Defn.Trait] = Print { t =>
    p(t.mods, "trait ", t.name, t.tparams, t.templ)
  }
  implicit val printDefnObject: Print[Defn.Object] = Print { t =>
    p(t.mods, "object ", t.name, t.templ)
  }

  // Ctor
  implicit val printCtorPrimary: Print[Ctor.Primary] = Print { t =>
    p(t.mods, (t.explicits, t.implicits))
  }
  implicit val printCtorSecondary: Print[Ctor.Secondary] = Print { t =>
    p(t.mods, "def this", (t.explicits, t.implicits), t.stats match {
      case Nil   => p(" = this", t.primaryCtorArgss)
      case stats => p("{ this", t.primaryCtorArgss, ";", r(stats, "; "), " }")
    })
  }

  // Pkg
  implicit val printPkgEmpty: Print[Pkg.Empty] = Print { t =>
    r(t.stats, "; ")
  }
  implicit val printPkgNamed: Print[Pkg.Named] = Print { t =>
    p("package ", t.name, " { ", r(t.stats, "; "), " }")
  }
  implicit val printPkgObject: Print[Pkg.Object] = Print { t =>
    p(t.mods, " package object ", t.name, " extends ", t.templ)
  }

  // Mod
  implicit val printModAnnot: Print[Mod.Annot] = Print { t => p("@", t.tpe, t.argss) }
  implicit val printModAbstract: Print[Mod.Abstract] = Print { _ => "abstract" }
  implicit val printModCase: Print[Mod.Case] = Print { _ => "case" }
  implicit val printModContravariant: Print[Mod.Contravariant] = Print { _ => "-" }
  implicit val printModCovariant: Print[Mod.Covariant] = Print { _ => "+" }
  implicit val printModDoc: Print[Mod.Doc] = Print { _ => ??? }
  implicit val printModFinal: Print[Mod.Final] = Print { _ => "final" }
  implicit val printModImplicit: Print[Mod.Implicit] = Print { _ => "implicit" }
  implicit val printModLazy: Print[Mod.Lazy] = Print { _ => "lazy" }
  implicit val printModMacro: Print[Mod.Macro] = Print { _ => "macro" }
  implicit val printModOverride: Print[Mod.Override] = Print { _ => "override" }
  implicit val printModSealed: Print[Mod.Sealed] = Print { _ => "sealed" }
  implicit val printModPrivate: Print[Mod.Private] = Print { t => p("private", t.within) }
  implicit val printModProtected: Print[Mod.Protected] = Print { t => p("protected", t.within) }
  implicit val printModValParam: Print[Mod.ValParam] = Print { _ => "val" }
  implicit val printModVarParam: Print[Mod.VarParam] = Print { _ => "var" }

  // Pat
  implicit val printPatAlternative: Print[Pat.Alternative] = Print { t =>
    p(t.lhs, " | ", t.rhs)
  }
  implicit val printPatBind: Print[Pat.Bind] = Print { t =>
    p(t.lhs, " @ ", t.rhs)
  }
  implicit val printPatExtract: Print[Pat.Extract] = Print { t =>
    p(t.ref, t.targs, t.elements)
  }
  implicit val printPatTuple: Print[Pat.Tuple] = Print { t =>
    p(t.elements)
  }
  implicit val printPatInterpolate: Print[Pat.Interpolate] = Print { t =>
    val zipped = t.parts.zip(t.args).map {
      case (part, id: Ident) if !id.isBackquoted => p(part, "$", id.value)
      case (part, arg)                          => p(part, "${", arg, "}")
    }
    p(t.prefix, "\"", r(zipped, ""), t.parts.last, "\"")
  }
  implicit val printPatWildcard: Print[Pat.Wildcard] = Print { _ => "_" }
  implicit val printPatSeqWildcard: Print[Pat.SeqWildcard] = Print { _ => "_*" }
  implicit val printPatTyped: Print[Pat.Typed] = Print { t =>
    p(t.lhs, ": ", t.rhs)
  }

  // Enum
  implicit val printEnumVal: Print[Enum.Val] = Print { t =>
    p(t.pat, " = ", t.rhs)
  }
  implicit val printEnumGenerator: Print[Enum.Generator] = Print { t =>
    p(t.pat, " <- ", t.rhs)
  }
  implicit val printEnumGuard: Print[Enum.Guard] = Print { t =>
    p("if ", t.cond)
  }

  // Aux and Misc
  implicit val printArgNamed: Print[Arg.Named] = Print { t =>
    p(t.name, " = ", t.rhs)
  }
  implicit val printArgRepeated: Print[Arg.Repeated] = Print { t =>
    p(t.arg, ": _*")
  }
  implicit val printCase: Print[Case] = Print { t =>
    p("case ", t.pat, t.cond.map { cond => p("if ", cond, " ") }.getOrElse(""), " =>", t.body.map(p(" ", _)).getOrElse(""))
  }
  implicit val printString: Print[String] = Print { identity }
  implicit val printIdent: Print[Ident] = Print { t =>
    if (t.isBackquoted) p("`", t.value, "`") else t.value
  }
  implicit val printAccessQualifier: Print[Mod.AccessQualifier] = Print {
    case t: Term.This => p(t)
    case t: Ident => p(t)
  }
  implicit val printTypeBounds: Print[TypeBounds] = Print { t =>
    p(t.lo.map { lo => p(" >: ", lo) }.getOrElse(""),
      t.hi.map { hi => p(" <: ", hi) }.getOrElse(""))
  }
  implicit val printParam: Print[Param] = Print { t =>
    p(t.mods, t.name, t.decltpe, t.default.map(p(" = ", _)).getOrElse(""))
  }
  implicit val printTemplate: Print[Template] = Print { t =>
    if (t eq Template.empty) ""
    else {
      val searly = if (t.early.isEmpty) "" else p("{ ", r(t.early, "; "), " } with ")
      val sbody = if ((t.self eq Self.empty) && t.stats.isEmpty) ""
                  else p("{ ", t.self, r(t.stats, "; "), " }")
      p(" ", searly, r(t.parents, " with "), sbody)
    }
  }
  implicit val printSelf: Print[Self] = Print { t =>
    if (t.name.isEmpty && t.tpe.isEmpty) ""
    else p(t.name, t.tpe, " => ")
  }
  implicit val printParent: Print[Parent] = Print { t => p(t.tpe, t.argss) }
  implicit val printTypeParam: Print[TypeParam] = Print { t =>
    val cbounds = r(t.contextBounds.map { p(": ", _) }, "")
    val vbounds = r(t.contextBounds.map { p("<% ", _) }, "")
    p(t.mods, t.name.map(p(_)).getOrElse("_"), t.tparams, cbounds, vbounds, t.bounds)
  }

  // Multiples and optionals
  implicit val printAccessQualifierOpt: Print[Option[Mod.AccessQualifier]] = Print { t =>
    t.map { qual => p("[", qual, "]") }.getOrElse("")
  }
  implicit val printArgs: Print[List[Arg]] = Print { args =>
    p("(", r(args, ", "), ")")
  }
  implicit val printArgss: Print[List[List[Arg]]] = Print { r(_, "") }
  implicit val printTargs: Print[List[Type]] = Print { targs =>
    if (targs.isEmpty) ""
    else p("[", r(targs, ", "), "]")
  }
  implicit val printPats: Print[List[Pat]] = Print { pats =>
    p("(", r(pats, ", "), ")")
  }
  implicit val printMods: Print[List[Mod]] = Print { mods =>
    if (mods.nonEmpty) p(r(mods, " "), " ") else ""
  }
  implicit val printParams: Print[List[Param]] = Print { params => p("(", r(params, ", "), ")") }
  implicit val printTparams: Print[List[TypeParam]] = Print { tparams =>
    if (tparams.nonEmpty) p("[", r(tparams, ", "), "]") else ""
  }
  implicit val printParamLists: Print[(List[List[Param]], List[Param])] = Print { case (expl, impl) =>
    p(r(expl, ""),
      if (impl.isEmpty) ""
      else p("(implicit ", r(impl, ", "), ")"))
  }
  implicit val printParamTypeOpt: Print[Option[ParamType]] = Print { _.map { t => p(": ", t) }.getOrElse("") }
  implicit val printTypeOpt: Print[Option[Type]] = Print { _.map { t => p(": ", t) }.getOrElse("") }
  implicit val printTermNameOpt: Print[Option[Term.Ident]] = Print { _.map(p(_)).getOrElse(")") }
  implicit val printImportSels: Print[List[Import.Selector]] = Print {
    case (t: Import.Selector.Name) :: Nil     => p(t)
    case (t: Import.Selector.Wildcard) :: Nil => p(t)
    case sels                                 => p("{ ", r(sels, ", "), " }")
  }

  // hierarchy is broken somewhere
  implicit def printStmtBlock: Print[Stmt.Block] = Print {
    case t: Import => p(t)
    case t: Term   => p(t)
    case t: Defn   => p(t)
  }
  implicit def printStmtRefine: Print[Stmt.Refine] = Print {
    case t: Decl => p(t)
  }
  implicit def printStmtExistential: Print[Stmt.Existential] = Print {
    case t: Decl.Val => p(t)
    case t: Decl.Type => p(t)
  }
  implicit def printStmtTopLevel: Print[Stmt.TopLevel] = Print {
    case t: Pkg.Named => p(t)
    case t: Pkg.Object => p(t)
    case t: Defn.Class => p(t)
    case t: Defn.Trait => p(t)
    case t: Defn.Object => p(t)
    case t: Import => p(t)
  }
  implicit def printStmtTemplate: Print[Stmt.Template] = Print {
    case t: Defn => p(t)
    case t: Decl => p(t)
    case t: Ctor.Secondary => p(t)
    case t: Import => p(t)
    case t: Term  => p(t)
  }
}
