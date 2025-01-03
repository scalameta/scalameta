package org.scalameta.adt

import org.scalameta.adt.Metadata.Adt
import scala.meta.internal.trees.AstNamerMacros
import scala.meta.internal.trees.{Reflection => AdtReflection}

import scala.language.experimental.macros
import scala.language.implicitConversions
import scala.reflect.macros.blackbox.Context

object TreeLiftsGenerate {
  def materializeAst[T <: Adt](isPrivateOKExpr: Boolean): String = macro TreeLiftsGenerateMacros
    .impl[T]
}

// Generates what Liftables.materializeAdt generates for Scala 2, but for Scala 3.
// In both cases, code meant to be used as part of the macro is generated.
// In Scala 2's Liftables.materializeAdt, a macro generates trees inlined directly
// into another macro code, which reference other methods from that macro code.
// Here, we instead generate a scala file for use as part of the macro, and we
// expose these methods via the TreeLiftsTrait trait.
class TreeLiftsGenerateMacros(val c: Context) extends AdtReflection {
  lazy val u: c.universe.type = c.universe
  lazy val mirror: u.Mirror = c.mirror
  import c.universe._

  lazy val TermApplySymbol = c.mirror.staticModule("scala.meta.Term").info.member(TypeName("Apply"))
    .asClass
  lazy val DefnValSymbol = c.mirror.staticModule("scala.meta.Defn").info.member(TypeName("Val"))
    .asClass
  lazy val DefnVarSymbol = c.mirror.staticModule("scala.meta.Defn").info.member(TypeName("Var"))
    .asClass
  lazy val PatBindSymbol = c.mirror.staticModule("scala.meta.Pat").info.member(TypeName("Bind"))
    .asClass
  lazy val PatTypedSymbol = c.mirror.staticModule("scala.meta.Pat").info.member(TypeName("Typed"))
    .asClass

  def customAdts(root: Root): Option[List[Adt]] = {
    val nonQuasis = root.allLeafs.filter(leaf => !(leaf.tpe <:< QuasiSymbol.toType))
    Some(QuasiSymbol.asBranch +: nonQuasis)
  }
  def customMatcher(adt: Adt, defName: String, localName: TermName): Option[String] = None
  def customWrapper(
      adt: Adt,
      defName: String,
      localName: TermName,
      body: String
  ): Option[String] = {
    def specialcaseTermApply: String =
      s"""|
          |    object ApplyToTripleDots extends scala.AnyRef {
          |      def unapply(t: scala.meta.Term.Apply): Option[scala.Tuple2[scala.meta.Term, scala.meta.Term.ArgClause.Quasi]] = t.argClause match {
          |        case (arg @ (_: scala.meta.Term.ArgClause.Quasi)) if arg.rank == 1 => scala.Some(scala.Tuple2(t.fun, arg))
          |        case _ => scala.None
          |      }
          |    }
          |    @tailrec() def checkNoTripleDots(fn: scala.meta.Term, arg: scala.meta.internal.trees.Quasi): Unit = fn match {
          |      case (t @ (_: scala.meta.Term.Apply)) => ApplyToTripleDots.unapply(t) match {
          |        case scala.None => checkNoTripleDots(t.fun, arg)
          |        case _ => report.errorAndAbort("rank mismatch when unquoting")
          |      }
          |      case _ => () // do nothing
          |    }
          |    def applyArgClauseQuasi(fn: _root_.scala.meta.Term)(arg: _root_.scala.meta.Term.ArgClause.Quasi) = {
          |      checkNoTripleDots(fn, arg)
          |      treeByMode('{scala.meta.internal.trees.Syntactic.TermApply.ArgList}, None, term(fn), term(List(arg)))
          |    }
          |    $localName match {
          |      case ApplyToTripleDots(fn, acq) => applyArgClauseQuasi(fn)(acq)
          |      case _ => $body
          |    }
          |""".stripMargin
    if (adt.tpe <:< QuasiSymbol.toType) Some(s"liftQuasi0($localName)")
    else if (adt.tpe <:< TermApplySymbol.toType) Some(specialcaseTermApply)
    else if (adt.tpe <:< DefnValSymbol.toType)
      Some(s"{ $localName.pats.foreach{pat => ${prohibitName(s"pat")}}\n    $body }")
    else if (adt.tpe <:< DefnVarSymbol.toType)
      Some(s"{ $localName.pats.foreach{pat => ${prohibitName(s"pat")}}\n    $body }")
    else if (adt.tpe <:< PatBindSymbol.toType)
      Some(s"{ ${prohibitName(s"$localName.lhs")}\n    $body }")
    else if (adt.tpe <:< PatTypedSymbol.toType)
      Some(s"{ ${prohibitName(s"$localName.lhs")}\n    $body }")
    else None
  }

  def impl[T: WeakTypeTag](isPrivateOKExpr: c.Expr[Boolean]): c.Expr[String] = {
    val isPrivateOK = c.eval(isPrivateOKExpr)
    val root = weakTypeOf[T].typeSymbol.asAdt.root
    val unsortedAdts = customAdts(root).getOrElse(root.allLeafs)
    val adts = unsortedAdts
    if (adts.isEmpty) {
      val message = s"materialization failed for Liftable[${weakTypeOf[T]}] " +
        s"(the most common reason for that is that you cannot materialize ADTs that haven't been compiled yet, " +
        s"i.e. materialization will fail if the file with ADT definitions comes after the file with the materialization call)"
      c.abort(c.enclosingPosition, message)
    }
    val u = q"${c.prefix}.u"
    val mainParam = c.freshName(TermName("x"))
    val mainModule = c.freshName(TermName("Module"))
    val mainMethod = TermName("liftableSub" + root.prefix.capitalize.replace(".", ""))
    val localName = c.freshName(TermName("x"))
    val defNames = adts.map(adt => "lift" + adt.prefix.capitalize.replace(".", ""))
    val liftAdts = adts.zip(defNames).map { case (adt, defName) =>
      val defaultBody: String = customMatcher(adt, defName, localName).getOrElse {
        val init = "_root_"
        def getNamePath(parts: Iterable[String]) = parts.foldLeft(init) { (acc, part) =>
          s"$acc.$part"
        }
        val nameParts = adt.sym.fullName.split('.')
        if (adt.sym.isClass) {
          val fields = adt match { case leaf: Leaf => leaf.fields(isPrivateOK); case _ => Nil }
          val args = fields.map(f => s"term($localName.${f.name})")
          val latestAfterVersion =
            if (adt.sym.isAstClass) {
              val moduleNames = adt.sym.companion.info.decls.flatMap { x =>
                if (x.isModule) Some(x.name.toString) else None
              }
              val latestAfterVersion = AstNamerMacros.getLatestAfterName(moduleNames).getOrElse {
                c.abort(c.enclosingPosition, s"no latest version ${adt.sym.fullName}: $moduleNames")
              }
              latestAfterVersion :: Nil
            } else Nil
          val namePath = getNamePath(nameParts ++ latestAfterVersion)
          val formattedArgs = (List(s"term($localName.origin)") ++ args).mkString(", ")
          s"treeByMode('{$namePath}, $formattedArgs)"
        } else getNamePath(nameParts)
      }
      val body = customWrapper(adt, defName, localName, defaultBody).getOrElse(defaultBody)
      s"def $defName($localName: ${adt.tpe}) = $body"
    }
    val clauses = adts.zip(defNames).map { case (adt, name) => s"case y : ${adt.tpe} => $name(y)" }

    val retStr =
      s"""|
          |package scala.meta
          |package internal
          |package quasiquotes
          |
          |import scala.runtime.ScalaRunTime
          |import scala.quoted._
          |import scala.meta.{Tree => MetaTree}
          |import scala.meta.internal.trees.Quasi
          |import scala.meta.internal.parsers.Absolutize._
          |import scala.meta.internal.parsers.Messages
          |
          |import scala.collection.mutable
          |import scala.annotation.tailrec
          |import scala.meta.trees.Origin
          |
          |$treeLiftsTrait
          |
          |trait TreeLifts(using override val internalQuotes: Quotes)(isPatternMode: Boolean, dialectExpr: Expr[Dialect]) extends ReificationMacros with TreeLiftsTrait {
          |  import internalQuotes.reflect._
          |$treeByMode
          |
          |$termMethods
          |
          |  ${liftAdts.mkString("\n  ")}
          |
          |  def liftableSubTree0[T <: MetaTree](y: T)(using Quotes): Tree = {
          |    y match {
          |      ${clauses.mkString("\n      ")}
          |      case _ => sys.error("none of leafs matched " + (y.getClass.getSimpleName))
          |    }
          |  }
          |}
          |""".stripMargin
    val retStrList = Literal(Constant(retStr))
    c.Expr[String](retStrList)
  }

  // Methods use holes in implementation, for which we do not have access here, so we implement those elsewhere
  def treeLiftsTrait =
    """|
       |trait TreeLiftsTrait(using val internalQuotes: Quotes) {
       |  def liftTree(tree: MetaTree): internalQuotes.reflect.Tree
       |  def liftOptionTree[T: Type](maybeTree: Option[MetaTree]): internalQuotes.reflect.Tree
       |  def liftTrees[T: Type](trees: Seq[MetaTree]): internalQuotes.reflect.Tree
       |  def liftTreess(treess: List[List[MetaTree]]): internalQuotes.reflect.Tree
       |  def liftQuasi0(quasi: Quasi, optional: Boolean = false): internalQuotes.reflect.Tree
       |  def liftOrigin(origin: Origin): internalQuotes.reflect.Tree
       |
       |  protected def unquotesName(q: scala.meta.internal.trees.Quasi): Boolean
       |}
       |""".stripMargin

  def treeByMode =
    """|
       |  def treeByMode[T](expr: Expr[T], origin: Option[Tree], args: Tree*): Tree =
       |    val term = expr.asTerm match
       |      case Inlined(_, _, inlined) => inlined
       |    def getResultType(x: TypeRepr, hasImplicit: Boolean): (TypeRepr, Boolean) = x match
       |      case mType @ MethodType(_, _, res) => getResultType(res, hasImplicit || mType.isImplicit)
       |      case res => (res, hasImplicit)
       |    val apply = term.symbol.methodMember("apply").head
       |    val (resType, hasImplicit) = getResultType(expr.asTerm.tpe.memberType(apply), hasImplicit = false)
       |    resType.asType match
       |      case '[t] =>
       |        if isPatternMode then
       |          TypedOrTest(Unapply(Select.unique(term, "unapply"), Nil, args.toList), TypeTree.of[t])
       |        else 
       |          val applied = Select.overloaded(term, "apply", Nil, (origin.map(List(_)).getOrElse(Nil) ++ args.toList).asInstanceOf[List[Term]])
       |          applied.tpe match
       |            case MethodType(_, _, _) => Apply(applied, List(dialectExpr.asTerm))
       |            case _ => applied
       |""".stripMargin

  def termMethods =
    """|  def term[T <: MetaTree](tree: T): Tree = liftableSubTree0(tree)
       |  def term[T <: MetaTree: Type](tree: Seq[T]): Tree = liftTrees[T](tree)
       |  def term[T <: MetaTree: Type](tree: List[T]): Tree = liftTrees[T](tree)
       |  @scala.annotation.targetName("term2") def term[T <: MetaTree: Type](tree: Seq[List[T]]): Tree = liftTreess(tree.toList)
       |  @scala.annotation.targetName("term3") def term[T <: MetaTree: Type](tree: List[List[T]]): Tree = liftTreess(tree)
       |  def term[T <: MetaTree: Type](tree: Option[T]): Tree = liftOptionTree[T](tree)
       |  def term(tree: Origin): Option[Tree] = if (isPatternMode) None else Some(liftOrigin(tree))
       |
       |  def term(tree: String): Tree = Literal(StringConstant(tree))
       |  def term(tree: Byte): Tree = Literal(ByteConstant(tree))
       |  def term(tree: Int): Tree = Literal(IntConstant(tree))
       |  def term(tree: Long): Tree = Literal(LongConstant(tree))
       |  def term(tree: Boolean): Tree = Literal(BooleanConstant(tree))
       |  def term(tree: scala.Symbol): Tree = '{scala.Symbol(${Expr(tree.name)})}.asTerm//Apply(Literal(StringConstant(tree)) 
       |""".stripMargin

  private def prohibitName(pat: String): String =
    s"""|
        |    import internalQuotes.reflect._
        |    def prohibitName(pat: _root_.scala.meta.Tree): _root_.scala.Unit = {
        |      pat match {
        |        case q: _root_.scala.meta.internal.trees.Quasi if unquotesName(q) =>
        |          val action = if (q.rank == 0) "unquote" else "splice"
        |          report.errorAndAbort("can't " + action + " a name here, use a pattern instead (e.g. p\\\"x\\\")")
        |        case _ =>
        |      }
        |    }
        |    prohibitName($pat)
        |""".stripMargin
}
