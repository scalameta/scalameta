package org.scalameta.adt

import org.scalameta.adt.Metadata.Adt
import scala.meta.internal.trees.{AstNamerMacros, CommonNamerMacros, Reflection => AdtReflection}

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
class TreeLiftsGenerateMacros(val c: Context) extends AdtReflection with CommonNamerMacros {
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

  private lazy val privateFields = getPrivateFields(TypeName(c.freshName())).asList
    .collect { case PrivateField(field, version) if version ne null => field.name }

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
          |      import sm.Term.ArgClause.{Quasi => ArgClauseQuasi}
          |      def unapply(t: sm.Term.Apply): Option[scala.Tuple2[sm.Term, ArgClauseQuasi]] = t.argClause match {
          |        case arg: ArgClauseQuasi if arg.rank == 1 => scala.Some(scala.Tuple2(t.fun, arg))
          |        case _ => scala.None
          |      }
          |    }
          |    @tailrec def checkNoTripleDots(fn: sm.Term): Unit = fn match {
          |      case t: sm.Term.Apply => ApplyToTripleDots.unapply(t) match {
          |        case scala.None => checkNoTripleDots(t.fun)
          |        case _ => report.errorAndAbort("rank mismatch when unquoting")
          |      }
          |      case _ => () // do nothing
          |    }
          |    $localName match {
          |      case ApplyToTripleDots(fn, acq) =>
          |        checkNoTripleDots(fn)
          |        treeByMode('{sm.internal.trees.Syntactic.TermApply.ArgList})(term(fn), term(List(acq)))
          |      case _ => $body
          |    }
          |""".stripMargin
    if (adt.tpe <:< QuasiSymbol.toType) Some(s"liftQuasi0($localName)")
    else if (adt.tpe <:< TermApplySymbol.toType) Some(specialcaseTermApply)
    else if (adt.tpe <:< DefnValSymbol.toType)
      Some(s"{ $localName.pats.foreach(prohibitName)\n    $body }")
    else if (adt.tpe <:< DefnVarSymbol.toType)
      Some(s"{ $localName.pats.foreach(prohibitName)\n    $body }")
    else if (adt.tpe <:< PatBindSymbol.toType) Some(s"{ prohibitName($localName.lhs)\n    $body }")
    else if (adt.tpe <:< PatTypedSymbol.toType) Some(s"{ prohibitName($localName.lhs)\n    $body }")
    else None
  }

  def impl[T: WeakTypeTag](isPrivateOKExpr: c.Expr[Boolean]): c.Expr[String] = {
    val isPrivateOK = c.eval(isPrivateOKExpr)
    val root = weakTypeOf[T].typeSymbol.asAdt.root
    val unsortedAdts = customAdts(root).getOrElse(root.allLeafs)
    if (unsortedAdts.isEmpty) {
      val message = s"materialization failed for Liftable[${weakTypeOf[T]}] " +
        s"(the most common reason for that is that you cannot materialize ADTs that haven't been compiled yet, " +
        s"i.e. materialization will fail if the file with ADT definitions comes after the file with the materialization call)"
      c.abort(c.enclosingPosition, message)
    }
    val localName = TermName("_x")
    def getArgs(fields: List[TermName]) = fields.map(f => s"$localName.$f").mkString(",")
    val privateArgs = getArgs(privateFields)
    val adts = unsortedAdts.map(adt => adt -> ("lift" + adt.prefix.capitalize.replace(".", "")))
    val liftAdts = adts.map { case (adt, defName) =>
      val defaultBody: String = customMatcher(adt, defName, localName).getOrElse {
        def getNamePath(parts: Iterable[String]) = {
          val name = parts.mkString(".")
          val smname = name.stripPrefix("scala.meta.")
          if (smname ne name) "sm." + smname else "_root_." + name
        }
        val nameParts = adt.sym.fullName.split('.')
        if (adt.sym.isClass) {
          val args = getArgs(adt match {
            case leaf: Leaf => leaf.fields(isPrivateOK).map(_.name)
            case _ => Nil
          })
          val latestAfterVersion =
            if (adt.sym.isAstClass) {
              val moduleNames = adt.sym.companion.info.decls
                .flatMap(x => if (x.isModule) Some(x.name.toString) else None)
              val latestAfterVersion = AstNamerMacros.getLatestAfterName(moduleNames).getOrElse(
                c.abort(c.enclosingPosition, s"no latest version ${adt.sym.fullName}: $moduleNames")
              )
              latestAfterVersion :: Nil
            } else Nil
          val namePath = getNamePath(nameParts ++ latestAfterVersion)
          s"treeByMode('{$namePath}, $privateArgs)($args)"
        } else getNamePath(nameParts)
      }
      val body = customWrapper(adt, defName, localName, defaultBody).getOrElse(defaultBody)
      s"def $defName($localName: ${adt.tpe}) = $body"
    }
    val clauses = adts.map { case (adt, name) => s"case y : ${adt.tpe} => $name(y)" }

    val retStr =
      s"""|package scala.meta
          |package internal
          |package quasiquotes
          |
          |import scala.runtime.ScalaRunTime
          |import scala.quoted._
          |import scala.{meta => sm}
          |import sm.internal.trees.Quasi
          |import sm.internal.parsers.Absolutize._
          |import sm.internal.parsers.Messages
          |import sm.trees.Origin
          |
          |import scala.collection.mutable
          |import scala.annotation.tailrec
          |import scala.language.implicitConversions
          |
          |$treeLiftsTrait
          |
          |trait TreeLifts(using quotes: Quotes)(isPatternMode: Boolean, dialectExpr: Expr[Dialect])
          |  extends ReificationMacros with TreeLiftsTrait
          |{
          |  import internalQuotes.reflect._
          |$treeByMode
          |
          |$termMethods
          |
          |  ${liftAdts.mkString("\n  ")}
          |
          |  def liftableSubTree0[T <: sm.Tree](y: T)(using Quotes): Tree = y match {
          |    ${clauses.mkString("\n    ")}
          |    case _ => sys.error("none of leafs matched " + y.getClass.getSimpleName)
          |  }
          |
          |  private def prohibitName(pat: sm.Tree): _root_.scala.Unit = pat match {
          |    case q: Quasi if unquotesName(q) =>
          |      val action = if (q.rank == 0) "unquote" else "splice"
          |      report.errorAndAbort("can't " + action + " a name here, use a pattern instead (e.g. p\\\"x\\\")")
          |    case _ =>
          |  }
          |}
          |""".stripMargin
    val retStrList = Literal(Constant(retStr))
    c.Expr[String](retStrList)
  }

  // Methods use holes in implementation, for which we do not have access here, so we implement those elsewhere
  def treeLiftsTrait =
    """|
       |trait TreeLiftsTrait extends HasInternalQuotes {
       |  import internalQuotes.{reflect => qr}
       |  def liftTree(tree: sm.Tree): qr.Tree
       |  def liftOptionTree[T: Type](maybeTree: Option[sm.Tree]): qr.Tree
       |  def liftTrees[T: Type](trees: Seq[sm.Tree]): qr.Tree
       |  def liftTreess(treess: List[List[sm.Tree]]): qr.Tree
       |  def liftQuasi0(quasi: Quasi, optional: Boolean = false): qr.Tree
       |  def liftOrigin(origin: Origin): qr.Tree
       |  protected def unquotesName(q: Quasi): Boolean
       |}
       |""".stripMargin

  def treeByMode =
    """|
       |  def treeByMode[T](expr: Expr[T], privateArgs: Tree*)(args: Tree*): Tree =
       |    val term = expr.asTerm match
       |      case Inlined(_, _, inlined) => inlined
       |    @tailrec
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
       |          val applied = Select.overloaded(term, "apply", Nil, (privateArgs ++ args).toList.asInstanceOf[List[Term]])
       |          applied.tpe match
       |            case MethodType(_, _, _) => Apply(applied, List(dialectExpr.asTerm))
       |            case _ => applied
       |""".stripMargin

  def termMethods =
    """|  implicit def term[T <: sm.Tree](tree: T): Tree = liftableSubTree0(tree)
       |  implicit def term[T <: sm.Tree: Type](tree: Seq[T]): Tree = liftTrees[T](tree)
       |  implicit def term[T <: sm.Tree: Type](tree: List[T]): Tree = liftTrees[T](tree)
       |  @scala.annotation.targetName("term2")
       |  implicit def term[T <: sm.Tree: Type](tree: Seq[List[T]]): Tree = liftTreess(tree.toList)
       |  @scala.annotation.targetName("term3")
       |  implicit def term[T <: sm.Tree: Type](tree: List[List[T]]): Tree = liftTreess(tree)
       |  implicit def term[T <: sm.Tree: Type](tree: Option[T]): Tree = liftOptionTree[T](tree)
       |  implicit def term(tree: Origin): Tree = liftOrigin(tree)
       |  implicit def term(tree: String): Tree = Literal(StringConstant(tree))
       |  implicit def term(tree: Char): Tree = Literal(CharConstant(tree))
       |  implicit def term(tree: Byte): Tree = Literal(ByteConstant(tree))
       |  implicit def term(tree: Short): Tree = Literal(ShortConstant(tree))
       |  implicit def term(tree: Int): Tree = Literal(IntConstant(tree))
       |  implicit def term(tree: Long): Tree = Literal(LongConstant(tree))
       |  implicit def term(tree: Boolean): Tree = Literal(BooleanConstant(tree))
       |  implicit def term(tree: scala.Symbol): Tree =
       |    '{scala.Symbol(${Expr(tree.name)})}.asTerm//Apply(Literal(StringConstant(tree)) 
       |""".stripMargin

}
