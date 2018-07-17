package scala.meta.internal.semanticdb.scalac

import scala.{meta => m}
import java.io.PrintWriter
import java.io.StringWriter
import java.io.Writer
import scala.collection.mutable
import scala.meta.internal.semanticdb.Scala.Symbols
import scala.reflect.internal.ModifierFlags._

trait PrinterOps { self: SemanticdbOps =>
  import g._

  def showSynthetic(tpe: g.Type): AttributedSynthetic = tpe match {
    case g.TypeBounds(_, _) =>
      // Skip signature for abstract type members, e.g. type T <: Int
      AttributedSynthetic.empty
    case PolyType(_, _: TypeBounds) =>
      // Type lambda with no body
      AttributedSynthetic.empty
    case _ => showSynthetic(g.TypeTree(tpe))
  }
  def showSynthetic(what: g.Tree): AttributedSynthetic = {
    val out = new StringWriter()
    val printer = SyntheticCodePrinter(out)
    printer.print(what)
    val occurrences = printer.occurrences.map {
      case ((start, end), symbol) => SyntheticRange(start, end, symbol)
    }.toList
    printer.occurrences.clear()
    val syntax = out.toString
    AttributedSynthetic(syntax, occurrences)
  }

  private object SyntheticCodePrinter {
    def apply(writer: Writer) = new SyntheticCodePrinter(new LengthWriter(writer, 0))
  }

  // An adaptation of g.CodePrinter that emits positioned symbols for names inside synthetics.
  // The modifications have been wrapped in "+- scalac deviation" comments.
  // In addition, the original source has been reformatted for better readability.
  private class SyntheticCodePrinter(out: LengthWriter) extends TreePrinter(new PrintWriter(out)) {

    // + scalac deviation
    case class ResolvedName(syntax: String, symbol: String)
    object ResolvedName {
      def apply(sym: g.Symbol): ResolvedName =
        ResolvedName(printedName(sym.name), sym.toSemantic)
    }
    val occurrences = mutable.HashMap.empty[(Int, Int), String]
    def printWithTrailingSpace(string: String): Unit =
      if (string.isEmpty) ()
      else {
        this.print(string)
        this.print(" ")
      }
    override def print(args: Any*): Unit = args.foreach {
      case ResolvedName(syntax, symbol) =>
        val start = out.length
        super.print(syntax)
        val end = out.length
        if (symbol != Symbols.None) {
          occurrences(start -> end) = symbol
        }
      case els => super.print(els)
    }
    // - scalac deviation

    // The original code printer was copied from the 2.11 sources and used mutable.Stack
    // which was deprecated in 2.12. I refactored the code to use a var + List instead.
    // + scalac deviation
    protected var parentsStack: List[g.Tree] = Nil
    // - scalac deviation

    private val printRootPkg = false

    protected def currentTree: Option[g.Tree] =
      parentsStack.headOption

    protected def currentParent: Option[g.Tree] =
      if (parentsStack.length > 1) Some(parentsStack(1)) else None

    protected def printedName(name: Name, decoded: Boolean = true): String = {
      import scala.reflect.internal.Chars._
      val decName = name.decoded
      val bslash = '\\'
      val isDot = (x: Char) => x == '.'
      val brackets = List('[', ']', '(', ')', '{', '}')

      def addBackquotes(s: String) =
        if (decoded &&
            (decName.exists(ch =>
              brackets.contains(ch) ||
                isWhitespace(ch) ||
                isDot(ch)) ||
            (name.isOperatorName &&
            decName.exists(isOperatorPart) &&
            decName.exists(isScalaLetter) &&
            !decName.contains(bslash))))
          s"`$s`"
        else s

      if (name == nme.CONSTRUCTOR) "this"
      else addBackquotes(quotedName(name, decoded))
    }

    protected def isIntLitWithDecodedOp(qual: Tree, name: Name): Boolean = {
      val qualIsIntLit = qual match {
        case Literal(Constant(x: Int)) => true
        case _ => false
      }
      qualIsIntLit && name.isOperatorName
    }

    override protected val commentsRequired = true

    protected def needsParentheses(parent: Tree)(
        insideIf: Boolean = true,
        insideMatch: Boolean = true,
        insideTry: Boolean = true,
        insideAnnotated: Boolean = true,
        insideBlock: Boolean = true,
        insideLabelDef: Boolean = true,
        insideAssign: Boolean = true): Boolean = {
      parent match {
        case _: If => insideIf
        case _: Match => insideMatch
        case _: Try => insideTry
        case _: Annotated => insideAnnotated
        case _: Block => insideBlock
        case _: LabelDef => insideLabelDef
        case _: Assign => insideAssign
        case _ => false
      }
    }

    protected def checkForBlank(cond: Boolean): String = if (cond) " " else ""
    protected def blankForOperatorName(name: Name): String = checkForBlank(name.isOperatorName)
    protected def blankForName(name: Name): String =
      checkForBlank(name.isOperatorName || name.endsWith("_"))
    def render(
        what: Any,
        printTypes: BooleanFlag = None,
        printIds: BooleanFlag = None,
        printOwners: BooleanFlag = None,
        printKinds: BooleanFlag = None,
        printMirrors: BooleanFlag = None,
        printPositions: BooleanFlag = None): String = {
      val buffer = new StringWriter()
      val writer = new LengthWriter(buffer, out.length)
      val printer = new SyntheticCodePrinter(writer)
      printTypes.value.map(printTypes =>
        if (printTypes) printer.withTypes else printer.withoutTypes)
      printIds.value.map(printIds => if (printIds) printer.withIds else printer.withoutIds)
      printOwners.value.map(printOwners =>
        if (printOwners) printer.withOwners else printer.withoutOwners)
      printKinds.value.map(printKinds =>
        if (printKinds) printer.withKinds else printer.withoutKinds)
      printMirrors.value.map(printMirrors =>
        if (printMirrors) printer.withMirrors else printer.withoutMirrors)
      printPositions.value.map(printPositions =>
        if (printPositions) printer.withPositions else printer.withoutPositions)
      printer.print(what)
      writer.flush()
      buffer.toString
    }

    protected def resolveSelect(t: Tree): String = {
      t match {
        // case for: 1) (if (a) b else c).meth1.meth2 or 2) 1 + 5 should be represented as (1).+(5)
        case Select(qual, name)
            if (name.isTermName &&
              needsParentheses(qual)(insideLabelDef = false)) ||
              isIntLitWithDecodedOp(qual, name) =>
          s"(${resolveSelect(qual)}).${printedName(name)}"
        case Select(qual, name) if name.isTermName =>
          s"${resolveSelect(qual)}.${printedName(name)}"
        case Select(qual, name) if name.isTypeName =>
          s"${resolveSelect(qual)}#${blankForOperatorName(name)}%${printedName(name)}"
        case Ident(name) => printedName(name)
        case _ => render(t)
      }
    }

    object EmptyTypeTree {
      def unapply(tt: TypeTree): Boolean = tt match {
        case build.SyntacticEmptyTypeTree() if tt.wasEmpty || tt.isEmpty => true
        case _ => false
      }
    }

    protected def isEmptyTree(tree: Tree) =
      tree match {
        case EmptyTree | EmptyTypeTree() => true
        case _ => false
      }

    protected def originalTypeTrees(trees: List[Tree]) =
      trees.filter(!isEmptyTree(_)).map {
        case tt: TypeTree if tt.original != null => tt.original
        case tree => tree
      }

    val defaultClasses = List(tpnme.AnyRef, tpnme.Object)
    val defaultTraitsForCase = List(tpnme.Product, tpnme.Serializable)
    protected def removeDefaultTypesFromList(trees: List[Tree])(
        classesToRemove: List[Name] = defaultClasses)(traitsToRemove: List[Name]) = {
      def removeDefaultTraitsFromList(trees: List[Tree], traitsToRemove: List[Name]): List[Tree] =
        trees match {
          case Nil => trees
          case init :+ last =>
            last match {
              case Select(Ident(sc), name) if traitsToRemove.contains(name) && sc == nme.scala_ =>
                removeDefaultTraitsFromList(init, traitsToRemove)
              case _ => trees
            }
        }

      removeDefaultTraitsFromList(
        removeDefaultClassesFromList(trees, classesToRemove),
        traitsToRemove)
    }

    protected def removeDefaultClassesFromList(
        trees: List[Tree],
        classesToRemove: List[Name] = defaultClasses) =
      originalTypeTrees(trees).filter {
        case Select(Ident(sc), name) => !(classesToRemove.contains(name) && sc == nme.scala_)
        case tt: TypeTree if tt.tpe != null =>
          !(classesToRemove contains (newTypeName(tt.tpe.toString())))
        case _ => true
      }

    protected def syntheticToRemove(tree: Tree) =
      tree match {
        case _: ValDef | _: TypeDef => false // don't remove ValDef and TypeDef
        case md: MemberDef if md.mods.isSynthetic => true
        case _ => false
      }

    override def printOpt(prefix: String, tree: Tree) =
      if (!isEmptyTree(tree)) super.printOpt(prefix, tree)

    override def printColumn(ts: List[Tree], start: String, sep: String, end: String) = {
      super.printColumn(ts.filter(!syntheticToRemove(_)), start, sep, end)
    }

    def printFlags(mods: Modifiers, primaryCtorParam: Boolean = false): Unit = {
      val base = AccessFlags | OVERRIDE | ABSTRACT | FINAL | SEALED | LAZY
      val mask = if (primaryCtorParam) base else base | IMPLICIT

      val s = mods.flagString(mask)
      if (s != "") print(s"$s ")
      // case flag should be the last
      if (mods.isCase) print(mods.flagBitsToString(CASE) + " ")
      if (mods.isAbstractOverride) print("abstract override ")
    }

    override def printModifiers(tree: Tree, mods: Modifiers): Unit =
      printModifiers(mods, primaryCtorParam = false)

    def printModifiers(mods: Modifiers, primaryCtorParam: Boolean): Unit = {
      def modsAccepted =
        List(currentTree, currentParent).exists(_.exists({
          case _: ClassDef | _: ModuleDef | _: Template | _: PackageDef => true
          case _ => false
        }))

      if (currentParent.isEmpty || modsAccepted)
        printFlags(mods, primaryCtorParam)
      else
        List(IMPLICIT, CASE, LAZY, SEALED).foreach { flag =>
          if (mods.hasFlag(flag)) print(s"${mods.flagBitsToString(flag)} ")
        }
    }

    def printParam(tree: Tree, primaryCtorParam: Boolean): Unit =
      tree match {
        case vd @ ValDef(mods, name, tp, rhs) =>
          printPosition(tree)
          printAnnotations(vd)
          val mutableOrOverride = mods.isOverride || mods.isMutable
          val hideCtorMods = mods.isParamAccessor && mods.isPrivateLocal && !mutableOrOverride
          val hideCaseCtorMods = mods.isCaseAccessor && mods.isPublic && !mutableOrOverride

          if (primaryCtorParam && !(hideCtorMods || hideCaseCtorMods)) {
            printModifiers(mods, primaryCtorParam)
            print(if (mods.isMutable) "var " else "val ")
          }
          print(printedName(name), blankForName(name))
          printOpt(": ", tp)
          printOpt(" = ", rhs)
        case TypeDef(_, name, tparams, rhs) =>
          printPosition(tree)
          print(printedName(name))
          printTypeParams(tparams)
          print(rhs)
        case _ =>
          super.printParam(tree)
      }

    override def printParam(tree: Tree): Unit = {
      printParam(tree, primaryCtorParam = false)
    }

    protected def printArgss(argss: List[List[Tree]]): Unit =
      argss.foreach({ x: List[Tree] =>
        if (!(x.isEmpty && argss.size == 1)) printRow(x, "(", ", ", ")")
      })

    override def printAnnotations(tree: MemberDef) = {
      val annots = tree.mods.annotations
      annots.foreach { annot =>
        printAnnot(annot); print(" ")
      }
    }

    protected def printAnnot(tree: Tree) = {
      tree match {
        case treeInfo.Applied(core, _, argss) =>
          print("@")
          core match {
            case Select(New(tree), _) => print(tree)
            case _ =>
          }
          printArgss(argss)
        case _ => super.printTree(tree)
      }
    }

    // + scalac deviation
    object PathDependentPrefix {
      def unapply(arg: g.Type): Option[g.Symbol] = arg match {
        case SingleType(_, sym) if sym.isTerm && !sym.isModule =>
          Some(sym)
        case _ => None
      }
    }
    def printType(tpe: Type): Unit = {
      def wrapped[T](
          ts: List[T],
          open: String,
          close: String,
          forceOpen: Boolean = false,
          sep: String = ", ")(f: T => Unit): Unit = {
        if (ts.nonEmpty || forceOpen) {
          this.print(open)
          var first = true
          ts.foreach { t =>
            if (!first) {
              this.print(sep)
            }
            f(t)
            first = false
          }
          this.print(close)
        }
      }

      def printSymbol(s: g.Symbol): Unit = {
        this.print(s.varianceString)
        val nameString = s.nameString
        this.print(nameString)
        if (nameString.lastOption.exists(!_.isLetterOrDigit)) {
          this.print(" ")
        }
        this.print(": ")
        this.printType(s.typeSignature)
        this.print(s.flagsExplanationString)
      }
      tpe match {
        case MethodType(params, result) =>
          var isImplicit = false
          wrapped(params, "(", ")", forceOpen = true) { s =>
            if (!isImplicit && s.isImplicit) {
              this.print("implicit ")
              isImplicit = true
            }
            printSymbol(s)
          }
          if (!result.isInstanceOf[MethodType]) {
            // Only print `: ` before last curried parameter list.
            this.print(": ")
          }
          this.printType(result)
        case NullaryMethodType(result) =>
          printType(result)
        case PolyType(tparams, result) =>
          wrapped(tparams, "[", "] => ")(s => this.print(s.decodedName))
          this.printType(result)
        case SingleType(pre, sym) =>
          if (!sym.isStable) this.printType(pre)
          this.print(ResolvedName(sym))
          this.print(".type")
        case ThisType(sym) =>
          this.print(ResolvedName(sym))
          this.print(".this.type")
        case ByNameType(arg) =>
          this.print("=>")
          this.printType(arg)
        case RepeatedType(arg) =>
          this.printType(arg)
          this.print("*")
        case TypeRef(pre, sym, args) =>
          pre match {
            case PathDependentPrefix(sym) =>
              this.print(ResolvedName(sym))
              this.print(".")
            case ThisType(sym)
                if !sym.hasPackageFlag &&
                  !sym.isModuleOrModuleClass &&
                  !sym.isConstructor =>
              this.print(ResolvedName(sym))
              this.print(".this.")
            case TypeRef(_, _, _ :: _) =>
              this.printType(pre)
              this.print("#")
            case _ =>
          }
          this.print(ResolvedName(sym))
          wrapped(args, "[", "]")(printType)
        case AnnotatedType(annotations, tpe) =>
          this.printType(tpe)
        case OverloadedType(pre, alternatives) =>
          this.printType(pre)
        case NoType =>
        case ConstantType(_) =>
          this.printType(tpe.widen)
        case RefinedType(parents, decls) =>
          wrapped(parents, "", "", sep = " with ") { s =>
            this.printType(s)
          }
          this.print("{")
          wrapped(decls.toList, "", "", sep = "; ") { s =>
            import s._
            this.printWithTrailingSpace(s.flagString)
            this.printWithTrailingSpace(s.keyString)
            this.print(varianceString)
            this.print(ResolvedName(s))
            this.print(signatureString)
            this.print(flagsExplanationString)
          }
          this.print("}")
        case _ =>
          this.print(tpe.toString())
      }
    }
    // - scalac deviation

    override def printTree(tree: Tree): Unit = {
      parentsStack = tree :: parentsStack
      try {
        processTreePrinting(tree)
        printTypesInfo(tree)
      } finally {
        parentsStack = parentsStack.tail
      }
    }

    def processTreePrinting(tree: Tree): Unit = {
      tree match {
        // don't remove synthetic ValDef/TypeDef
        case _ if syntheticToRemove(tree) =>
        case cl @ ClassDef(mods, name, tparams, impl) =>
          if (mods.isJavaDefined) super.printTree(cl)
          printAnnotations(cl)
          // traits
          val clParents: List[Tree] = if (mods.isTrait) {
            // avoid abstract modifier for traits
            printModifiers(tree, mods &~ ABSTRACT)
            print("trait ", printedName(name))
            printTypeParams(tparams)

            val build.SyntacticTraitDef(_, _, _, _, parents, _, _) = tree
            parents
            // classes
          } else {
            printModifiers(tree, mods)
            print("class ", printedName(name))
            printTypeParams(tparams)

            val build.SyntacticClassDef(
              _,
              _,
              _,
              ctorMods,
              vparamss,
              earlyDefs,
              parents,
              selfType,
              body) = cl

            // constructor's modifier
            if (ctorMods.hasFlag(AccessFlags) || ctorMods.hasAccessBoundary) {
              print(" ")
              printModifiers(ctorMods, primaryCtorParam = false)
            }

            def printConstrParams(ts: List[ValDef]): Unit = {
              parenthesize() {
                printImplicitInParamsList(ts)
                printSeq(ts)(printParam(_, primaryCtorParam = true))(print(", "))
              }
            }
            // constructor's params processing (don't print single empty constructor param list)
            vparamss match {
              case Nil | List(Nil) if !mods.isCase && !ctorMods.hasFlag(AccessFlags) =>
              case _ => vparamss.foreach(printConstrParams)
            }
            parents
          }

          // get trees without default classes and traits (when they are last)
          val printedParents = removeDefaultTypesFromList(clParents)()(
            if (mods.hasFlag(CASE)) defaultTraitsForCase else Nil)
          print(
            if (mods.isDeferred) "<: " else if (printedParents.nonEmpty) " extends " else "",
            impl)

        case pd @ PackageDef(packaged, stats) =>
          packaged match {
            case Ident(name) if name == nme.EMPTY_PACKAGE_NAME =>
              printSeq(stats)(print(_)) {
                println()
                println()
              }
            case _ =>
              printPackageDef(pd, scala.util.Properties.lineSeparator)
          }

        case md @ ModuleDef(mods, name, impl) =>
          printAnnotations(md)
          printModifiers(tree, mods)
          val Template(parents, self, methods) = impl
          val parWithoutAnyRef = removeDefaultClassesFromList(parents)
          print(
            "object " + printedName(name),
            if (parWithoutAnyRef.nonEmpty) " extends " else "",
            impl)

        case vd @ ValDef(mods, name, tp, rhs) =>
          printValDef(vd, printedName(name)) {
            // place space after symbolic def name (val *: Unit does not compile)
            printOpt(s"${blankForName(name)}: ", tp)
          } {
            if (!mods.isDeferred) print(" = ", if (rhs.isEmpty) "_" else rhs)
          }

        case dd @ DefDef(mods, name, tparams, vparamss, tp, rhs) =>
          printDefDef(dd, printedName(name)) {
            if (tparams.isEmpty && (vparamss.isEmpty || vparamss(0).isEmpty))
              print(blankForName(name))
            printOpt(": ", tp)
          } {
            printOpt(" = " + (if (mods.isMacro) "macro " else ""), rhs)
          }

        case td @ TypeDef(mods, name, tparams, rhs) =>
          printTypeDef(td, printedName(name))

        case LabelDef(name, params, rhs) =>
          if (name.startsWith(nme.WHILE_PREFIX)) {
            val If(cond, thenp, elsep) = rhs
            print("while (", cond, ") ")
            val Block(list, wh) = thenp
            printColumn(list, "", ";", "")
          } else if (name.startsWith(nme.DO_WHILE_PREFIX)) {
            val Block(bodyList, ifCond @ If(cond, thenp, elsep)) = rhs
            print("do ")
            printColumn(bodyList, "", ";", "")
            print(" while (", cond, ") ")
          } else {
            print(printedName(name)); printLabelParams(params)
            printBlock(rhs)
          }

        case imp @ Import(expr, _) =>
          printImport(imp, resolveSelect(expr))

        case t @ Template(parents, self, tbody) =>
          val body = t.body // treeInfo.untypecheckedTemplBody(t)
          val printedParents =
            currentParent
              .map {
                case _: CompoundTypeTree => parents
                case ClassDef(mods, name, _, _) if mods.isCase =>
                  removeDefaultTypesFromList(parents)()(List(tpnme.Product, tpnme.Serializable))
                case _ => removeDefaultClassesFromList(parents)
              }
              .getOrElse(parents)

          val primaryCtr = treeInfo.firstConstructor(body)
          val ap: Option[Apply] = primaryCtr match {
            case DefDef(_, _, _, _, _, Block(ctBody, _)) =>
              val earlyDefs = treeInfo.preSuperFields(ctBody) ::: body.filter {
                case td: TypeDef => treeInfo.isEarlyDef(td)
                case _ => false
              }
              if (earlyDefs.nonEmpty) {
                print("{")
                printColumn(earlyDefs, "", ";", "")
                print("} " + (if (printedParents.nonEmpty) "with " else ""))
              }
              ctBody.collectFirst {
                case apply: Apply => apply
              }
            case _ => None
          }

          if (printedParents.nonEmpty) {
            val (clParent :: traits) = printedParents
            print(clParent)

            val constrArgss = ap match {
              case Some(treeInfo.Applied(_, _, argss)) => argss
              case _ => Nil
            }
            printArgss(constrArgss)
            if (traits.nonEmpty) {
              printRow(traits, " with ", " with ", "")
            }
          }
          /* Remove primary constr def and constr val and var defs
           * right contains all constructors
           */
          val (left, right) = body
            .filter {
              // remove valdefs defined in constructor and presuper vals
              case vd: ValDef => !vd.mods.isParamAccessor && !treeInfo.isEarlyValDef(vd)
              // remove $this$ from traits
              case dd: DefDef => dd.name != nme.MIXIN_CONSTRUCTOR
              case td: TypeDef => !treeInfo.isEarlyDef(td)
              case EmptyTree => false
              case _ => true
            }
            .span {
              case dd: DefDef => dd.name != nme.CONSTRUCTOR
              case _ => true
            }
          val modBody = left ::: right.drop(1)
          val showBody = !(modBody.isEmpty && (self == noSelfType || self.isEmpty))
          if (showBody) {
            if (self.name != nme.WILDCARD) {
              print(" { ", self.name)
              printOpt(": ", self.tpt)
              print(" =>")
            } else if (self.tpt.nonEmpty) {
              print(" { _ : ", self.tpt, " =>")
            } else {
              print(" {")
            }
            printColumn(modBody, "", ";", "}")
          }

        case bl @ Block(stats, expr) =>
          printBlock(bl.stats, expr)

        case Match(selector, cases) =>
          /* Insert braces if match is inner
           * make this function available for other cases
           * passing required type for checking
           */
          def insertBraces(body: => Unit): Unit =
            if (parentsStack.nonEmpty && parentsStack.tail.exists(_.isInstanceOf[Match])) {
              print("(")
              body
              print(")")
            } else body

          val printParentheses = needsParentheses(selector)(insideLabelDef = false)
          tree match {
            case Match(EmptyTree, cs) =>
              printColumn(cases, "{", "", "}")
            case _ =>
              insertBraces {
                parenthesize(printParentheses)(print(selector))
                printColumn(cases, " match {", "", "}")
              }
          }

        case cd @ CaseDef(pat, guard, body) =>
          printCaseDef(cd)

        case Star(elem) =>
          print(elem, "*")

        case Bind(name, t) =>
          if (t == EmptyTree) print("(", printedName(name), ")")
          else if (t.exists(_.isInstanceOf[Star])) print(printedName(name), " @ ", t)
          else print("(", printedName(name), " @ ", t, ")")

        case f @ Function(vparams, body) =>
          // parentheses are not allowed for val a: Int => Int = implicit x => x
          val printParentheses = vparams match {
            case head :: _ => !head.mods.isImplicit
            case _ => true
          }
          printFunction(f)(printValueParams(vparams, inParentheses = printParentheses))

        case Typed(expr, tp) =>
          def printTp = print("(", tp, ")")

          tp match {
            case EmptyTree | EmptyTypeTree() => printTp
            // case for untypechecked trees
            case Annotated(annot, arg)
                if (expr ne null) && (arg ne null) && expr.equalsStructure(arg) =>
              printTp // remove double arg - 5: 5: @unchecked
            case tt: TypeTree if tt.original.isInstanceOf[Annotated] => printTp
            case Function(List(), EmptyTree) => print("(", expr, " _)") //func _
            // parentheses required when (a match {}) : Type
            case _ => print("((", expr, "): ", tp, ")")
          }

        // print only fun when targs are TypeTrees with empty original
        case TypeApply(fun, targs) =>
          if (targs.exists(isEmptyTree)) {
            print(fun)
          } else super.printTree(tree)

        case Apply(fun, vargs) =>
          tree match {
            // processing methods ending on colons (x \: list)
            case Apply(
                Block(
                  l1 @ List(sVD: ValDef),
                  a1 @ Apply(Select(_, methodName), l2 @ List(Ident(iVDName)))),
                l3)
                if sVD.mods.isSynthetic && treeInfo.isLeftAssoc(methodName) && sVD.name == iVDName =>
              val printBlock = Block(l1, Apply(a1, l3))
              print(printBlock)
            case Apply(tree1, _) if needsParentheses(tree1)(insideAnnotated = false) =>
              parenthesize()(print(fun)); printRow(vargs, "(", ", ", ")")
            case _ => super.printTree(tree)
          }

        case UnApply(fun, args) =>
          fun match {
            case treeInfo.Unapplied(body) =>
              body match {
                case Select(qual, name) if name == nme.unapply => print(qual)
                case TypeApply(Select(qual, name), _)
                    if name == nme.unapply || name == nme.unapplySeq =>
                  print(qual)
                case _ => print(body)
              }
            case _ => print(fun)
          }
          printRow(args, "(", ", ", ")")

        case st @ Super(This(qual), mix) =>
          printSuper(st, printedName(qual), checkSymbol = false)

        case th @ This(qual) =>
          if (tree.hasExistingSymbol && tree.symbol.hasPackageFlag) print(tree.symbol.fullName)
          else printThis(th, printedName(qual))

        // remove this prefix from constructor invocation in typechecked trees: this.this -> this
        case Select(This(_), name @ nme.CONSTRUCTOR) => print(printedName(name))

        case Select(qual: New, name) =>
          print(qual)

        case sel @ Select(qual, name) =>
          def checkRootPackage(tr: Tree): Boolean =
            (currentParent match { //check that Select is not for package def name
              case Some(_: PackageDef) => false
              case _ => true
            }) && (tr match { // check that Select contains package
              case Select(q, _) => checkRootPackage(q)
              case _: Ident | _: This =>
                val sym = tr.symbol
                tr.hasExistingSymbol && sym.hasPackageFlag && sym.name != nme.ROOTPKG
              case _ => false
            })

          if (printRootPkg && checkRootPackage(tree)) print(s"${printedName(nme.ROOTPKG)}.")
          val printParentheses =
            needsParentheses(qual)(insideAnnotated = false) ||
              isIntLitWithDecodedOp(qual, name)

          // + scalac deviation
          val resolved = ResolvedName(printedName(name), sel.symbol.toSemantic)
          if (printParentheses) print("(", resolveSelect(qual), ").", resolved)
          else print(resolveSelect(qual), ".", resolved)
        // - scalac deviation

        case id @ Ident(name) =>
          if (name.nonEmpty) {
            if (name == nme.dollarScope) {
              print(s"scala.xml.${nme.TopScope}")
            } else {
              val str = printedName(name)
              val strIsBackquoted = str.startsWith("`") && str.endsWith("`")
              print(if (id.isBackquoted && !strIsBackquoted) "`" + str + "`" else str)
            }
          } else {
            print("")
          }

        case l @ Literal(x) =>
          import scala.reflect.internal.Chars.LF
          x match {
            case Constant(v: String) if {
                  val strValue = x.stringValue
                  strValue.contains(LF) &&
                  !strValue.contains("\"\"\"") &&
                  strValue.length > 1
                } =>
              val splitValue = x.stringValue.split(s"$LF").toList
              val multilineStringValue =
                if (x.stringValue.endsWith(s"$LF")) splitValue :+ "" else splitValue
              val trQuotes = "\"\"\""
              print(trQuotes)
              printSeq(multilineStringValue) { print(_) } { print(LF) }
              print(trQuotes)
            case _ =>
              // processing Float constants
              val printValue = x.escapedStringValue + (if (x.value.isInstanceOf[Float]) "F"
                                                       else "")
              print(printValue)
          }

        case an @ Annotated(ap, tree) =>
          val printParentheses = needsParentheses(tree)()
          parenthesize(printParentheses) { print(tree) }
          print(if (tree.isType) " " else ": ")
          printAnnot(ap)

        case SelectFromTypeTree(qualifier, selector) =>
          print("(", qualifier, ")#", blankForOperatorName(selector), printedName(selector))

        case tt: TypeTree =>
          if (!isEmptyTree(tt)) {
            val original = tt.original
            if (original != null) print(original)
            else {
              // + scalac deviation
              this.printType(tree.tpe)
              // - scalac deviation
            }
          }

        case AppliedTypeTree(tp, args) =>
          // it's possible to have (=> String) => String type but Function1[=> String, String] is not correct
          val containsByNameTypeParam = args.exists(treeInfo.isByNameParamType)

          if (containsByNameTypeParam) {
            print("(")
            printRow(args.init, "(", ", ", ")")
            print(" => ", args.last, ")")
          } else {
            if (treeInfo.isRepeatedParamType(tree) && args.nonEmpty) {
              print(args.head, "*")
            } else if (treeInfo.isByNameParamType(tree)) {
              print("=> ", if (args.isEmpty) "()" else args.head)
            } else
              super.printTree(tree)
          }

        case ExistentialTypeTree(tpt, whereClauses) =>
          print("(", tpt)
          printColumn(whereClauses, " forSome { ", ";", "})")

        case EmptyTree =>
        case tree => super.printTree(tree)
      }
    }
  }
}
