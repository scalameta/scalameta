package cbc

import Flags._
import scala.collection.mutable.ListBuffer
import StdNames._, Names._, FreshNames._, Trees._, Positions._, Constants._

object TreeGen {
  def rootId(name: Name)             = Select(Ident(nme.ROOTPKG), name)
  def rootScalaDot(name: Name)       = Select(rootId(nme.scala_), name)
  def scalaDot(name: Name)           = rootScalaDot(name)
  def scalaAnnotationDot(name: Name) = Select(scalaDot(nme.annotation), name)
  def scalaAny                       = scalaDot(tpnme.Any) // used in ide
  def scalaAnyRefConstr              = scalaDot(tpnme.AnyRef) // used in ide

  def mkUnattributedRef(fullName: Name): RefTree = {
    val hd :: tl = nme.segments(fullName.toString, assumeTerm = fullName.isTermName)
    tl.foldLeft(Ident(hd): RefTree)(Select(_,_))
  }

  /** Wrap an expression in a named argument. */
  def mkNamedArg(name: Name, tree: Tree): Tree = mkNamedArg(Ident(name), tree)
  def mkNamedArg(lhs: Tree, rhs: Tree): Tree = atPos(rhs.pos)(AssignOrNamedArg(lhs, rhs))

  /** Builds a tuple */
  def mkTuple(elems: List[Tree], flattenUnary: Boolean = true): Tree = elems match {
    case Nil =>
      Literal(Constant(()))
    case tree :: Nil if flattenUnary =>
      tree
    case _ =>
      Apply(scalaDot(TermName(s"Tuple${elems.length}")), elems)
  }

  def mkTupleType(elems: List[Tree], flattenUnary: Boolean = true): Tree = elems match {
    case Nil =>
      scalaDot(tpnme.Unit)
    case List(tree) if flattenUnary =>
      tree
    case _ =>
      AppliedTypeTree(scalaDot(TypeName(s"Tuple${elems.length}")), elems)
  }

  def mkSuperInitCall: Select = Select(Super(This(tpnme.EMPTY), tpnme.EMPTY), nme.CONSTRUCTOR)

  /** Generates a template with constructor corresponding to
   *
   *  constrmods (vparams1_) ... (vparams_n) preSuper { presupers }
   *  extends superclass(args_1) ... (args_n) with mixins { self => body }
   *
   *  This gets translated to
   *
   *  extends superclass with mixins { self =>
   *    presupers' // presupers without rhs
   *    vparamss   // abstract fields corresponding to value parameters
   *    def <init>(vparamss) {
   *      presupers
   *      super.<init>(args)
   *    }
   *    body
   *  }
   */
  def mkTemplate(parents: List[Tree], self: ValDef, constrMods: Modifiers,
                 vparamss: List[List[ValDef]], body: List[Tree], superPos: Position = NoPosition): Template = {
    /* Add constructor to template */

    // create parameters for <init> as synthetic trees.
    var vparamss1 = vparamss.map { _.map { vd =>
      val param = atPos(vd.pos.makeTransparent) {
        val mods = Modifiers(vd.mods.flags & (IMPLICIT | DEFAULTPARAM | BYNAMEPARAM) | PARAM | PARAMACCESSOR)
        ValDef(mods withAnnotations vd.mods.annotations, vd.name, vd.tpt.duplicate, duplicateAndKeepPositions(vd.rhs))
      }
      param
    }}

    val (edefs, rest) = body span TreeInfo.isEarlyDef
    val (evdefs, etdefs) = edefs partition TreeInfo.isEarlyValDef
    val gvdefs = evdefs map {
      case vdef @ ValDef(_, _, tpt, _) =>
        copyValDef(vdef)(
        // atPos for the new tpt is necessary, since the original tpt might have no position
        // (when missing type annotation for ValDef for example), so even though setOriginal modifies the
        // position of TypeTree, it would still be NoPosition. That's what the author meant.
        tpt = atPos(vdef.pos.focus)(TypeTree() setOriginal tpt setPos tpt.pos.focus),
        rhs = EmptyTree
      )
    }
    val lvdefs = evdefs collect { case vdef: ValDef => copyValDef(vdef)(mods = vdef.mods | PRESUPER) }

    val constr = {
      if (constrMods.isTrait) {
        if (body forall TreeInfo.isInterfaceMember) None
        else Some(
          atPos(wrappingPos(superPos, lvdefs)) (
            DefDef(NoMods, nme.MIXIN_CONSTRUCTOR, Nil, List(Nil), TypeTree(), Block(lvdefs, Literal(Constant())))))
      }
      else {
        // convert (implicit ... ) to ()(implicit ... ) if its the only parameter section
        if (vparamss1.isEmpty || !vparamss1.head.isEmpty && vparamss1.head.head.mods.isImplicit)
          vparamss1 = List() :: vparamss1
        val superCall = pendingSuperCall // we can't know in advance which of the parents will end up as a superclass
                                         // this requires knowing which of the parents is a type macro and which is not
                                         // and that's something that cannot be found out before typer
                                         // (the type macros aren't in the trunk yet, but there is a plan for them to land there soon)
                                         // this means that we don't know what will be the arguments of the super call
                                         // therefore here we emit a dummy which gets populated when the template is named and typechecked
        Some(
          atPos(wrappingPos(superPos, lvdefs ::: vparamss1.flatten).makeTransparent) (
            DefDef(constrMods, nme.CONSTRUCTOR, List(), vparamss1, TypeTree(), Block(lvdefs ::: List(superCall), Literal(Constant())))))
      }
    }
    constr foreach (ensureNonOverlapping(_, parents ::: gvdefs, focus = false))
    // Field definitions for the class - remove defaults.

    val fieldDefs = vparamss.flatten map (vd => {
      val field = copyValDef(vd)(mods = vd.mods &~ DEFAULTPARAM, rhs = EmptyTree)
      // Prevent overlapping of `field` end's position with default argument's start position.
      // This is needed for `Positions.Locator(pos).traverse` to return the correct tree when
      // the `pos` is a point position with all its values equal to `vd.rhs.pos.start`.
      if(field.pos.isRange && vd.rhs.pos.isRange) field.pos = field.pos.withEnd(vd.rhs.pos.start - 1)
      field
    })

    Template(parents, self, gvdefs ::: fieldDefs ::: constr ++: etdefs ::: rest)
  }

  def mkParents(ownerMods: Modifiers, parents: List[Tree], parentPos: Position = NoPosition) =
    if (ownerMods.isCase) parents ::: List(scalaDot(tpnme.Product), scalaDot(tpnme.Serializable))
    else if (parents.isEmpty) atPos(parentPos)(scalaAnyRefConstr) :: Nil
    else parents

  def mkClassDef(mods: Modifiers, name: TypeName, tparams: List[TypeDef], templ: Template): ClassDef = {
    val isInterface = mods.isTrait && (templ.body forall TreeInfo.isInterfaceMember)
    val mods1 = if (isInterface) (mods | Flags.INTERFACE) else mods
    ClassDef(mods1, name, tparams, templ)
  }

  /** Create positioned tree representing an object creation <new parents { stats }
   *  @param npos  the position of the new
   *  @param cpos  the position of the anonymous class starting with parents
   */
  def mkNew(parents: List[Tree], self: ValDef, stats: List[Tree],
            npos: Position, cpos: Position): Tree =
    if (parents.isEmpty)
      mkNew(List(scalaAnyRefConstr), self, stats, npos, cpos)
    else if (parents.tail.isEmpty && stats.isEmpty) {
      // `Parsers.template` no longer differentiates tpts and their argss
      // e.g. `C()` will be represented as a single tree Apply(Ident(C), Nil)
      // instead of parents = Ident(C), argss = Nil as before
      // this change works great for things that are actually templates
      // but in this degenerate case we need to perform postprocessing
      val app = TreeInfo.dissectApplied(parents.head)
      atPos(npos union cpos) { New(app.callee, app.argss) }
    } else {
      val x = tpnme.ANON_CLASS_NAME
      atPos(npos union cpos) {
        Block(
          List(
            atPos(cpos) {
              ClassDef(
                Modifiers(FINAL), x, Nil,
                mkTemplate(parents, self, NoMods, List(Nil), stats, cpos.focus))
            }),
          atPos(npos) {
            New(
              Ident(x) setPos npos.focus,
              Nil)
          }
        )
      }
    }

  /** Create a tree representing the function type (argtpes) => restpe */
  def mkFunctionTypeTree(argtpes: List[Tree], restpe: Tree): Tree =
    AppliedTypeTree(rootScalaDot(newTypeName("Function" + argtpes.length)), argtpes ::: List(restpe))

  /** Create a literal unit tree that is inserted by the compiler but not
   *  written by end user. It's important to distinguish the two so that
   *  quasiquotes can strip synthetic ones away.
   */
  def mkSyntheticUnit() = Literal(Constant(()))

  /** Create block of statements `stats`  */
  def mkBlock(stats: List[Tree]): Tree =
    if (stats.isEmpty) mkSyntheticUnit()
    else if (!stats.last.isTerm) Block(stats, mkSyntheticUnit())
    else if (stats.length == 1) stats.head
    else Block(stats.init, stats.last)

  /** Create a block that wraps multiple statements but don't
   *  do any wrapping if there is just one statement. Used by
   *  quasiquotes, macro c.parse api and toolbox.
   */
  def mkTreeOrBlock(stats: List[Tree]) = stats match {
    case Nil         => EmptyTree
    case head :: Nil => head
    case _           => mkBlock(stats)
  }

  /** Create a tree representing an assignment <lhs = rhs> */
  def mkAssign(lhs: Tree, rhs: Tree): Tree = lhs match {
    case Apply(fn, args) => Apply(atPos(fn.pos)(Select(fn, nme.update)), args :+ rhs)
    case _               => Assign(lhs, rhs)
  }

  def mkPackageObject(defn: ModuleDef, pidPos: Position = NoPosition, pkgPos: Position = NoPosition) = {
    val module = copyModuleDef(defn)(name = nme.PACKAGEkw)
    val pid    = atPos(pidPos)(Ident(defn.name))
    atPos(pkgPos)(PackageDef(pid, module :: Nil))
  }

  // Following objects represent encoding of for loop enumerators
  // into the regular trees. Such representations are used for:
  //
  //   - as intermediate value of enumerators inside of the parser
  //     right before the mkFor desugaring is being called
  //
  //   - as intermediate value of enumerators obtained after
  //     re-sugaring of for loops through build.SyntacticFor
  //     and build.SyntacticForYield (which are used by quasiquotes)
  //
  // The encoding uses regular trees with ForAttachment that helps
  // to reliably differentiate them from normal trees that can have
  // similar shape. fq"$pat <- $rhs" for example is represented in
  // the same way as "`<-`($pat, $rhs)"" but with added attachment to
  // the `<-` identifier.
  //
  // The primary rationale behind such representation in favor of
  // simple case classes is a wish to re-use the same representation
  // between quasiquotes and parser without exposing compiler internals.
  // Opaque tree encoding can be changed/adapted at any time without
  // breaking end users code.

  /** Encode/decode fq"$pat <- $rhs" enumerator as q"`<-`($pat, $rhs)" */
  object ValFrom {
    def apply(pat: Tree, rhs: Tree): Tree =
      Apply(Ident(nme.LARROWkw),
        List(pat, rhs))

    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case Apply(id @ Ident(nme.LARROWkw), List(pat, rhs)) =>
        Some((pat, rhs))
      case _ => None
    }
  }

  /** Encode/decode fq"$pat = $rhs" enumerator as q"$pat = $rhs" */
  object ValEq {
    def apply(pat: Tree, rhs: Tree): Tree =
      Assign(pat, rhs)

    def unapply(tree: Tree): Option[(Tree, Tree)] = tree match {
      case Assign(pat, rhs) =>
        Some((pat, rhs))
      case _ => None
    }
  }

  /** Encode/decode fq"if $cond" enumerator as q"`if`($cond)" */
  object Filter {
    def apply(tree: Tree) =
      Apply(Ident(nme.IFkw), List(tree))

    def unapply(tree: Tree): Option[Tree] = tree match {
      case Apply(id @ Ident(nme.IFkw), List(cond)) =>
        Some((cond))
      case _ => None
    }
  }

  /** Encode/decode body of for yield loop as q"`yield`($tree)" */
  object Yield {
    def apply(tree: Tree): Tree =
      Apply(Ident(nme.YIELDkw), List(tree))

    def unapply(tree: Tree): Option[Tree] = tree match {
      case Apply(id @ Ident(nme.YIELDkw), List(tree)) =>
        Some(tree)
      case _  => None
    }
  }

  /** Create tree for for-comprehension <for (enums) do body> or
  *   <for (enums) yield body> where mapName and flatMapName are chosen
  *  corresponding to whether this is a for-do or a for-yield.
  *  The creation performs the following rewrite rules:
  *
  *  1.
  *
  *    for (P <- G) E   ==>   G.foreach (P => E)
  *
  *     Here and in the following (P => E) is interpreted as the function (P => E)
  *     if P is a variable pattern and as the partial function { case P => E } otherwise.
  *
  *  2.
  *
  *    for (P <- G) yield E  ==>  G.map (P => E)
  *
  *  3.
  *
  *    for (P_1 <- G_1; P_2 <- G_2; ...) ...
  *      ==>
  *    G_1.flatMap (P_1 => for (P_2 <- G_2; ...) ...)
  *
  *  4.
  *
  *    for (P <- G; E; ...) ...
  *      =>
  *    for (P <- G.filter (P => E); ...) ...
  *
  *  5. For N < MaxTupleArity:
  *
  *    for (P_1 <- G; P_2 = E_2; val P_N = E_N; ...)
  *      ==>
  *    for (TupleN(P_1, P_2, ... P_N) <-
  *      for (x_1 @ P_1 <- G) yield {
  *        val x_2 @ P_2 = E_2
  *        ...
  *        val x_N & P_N = E_N
  *        TupleN(x_1, ..., x_N)
  *      } ...)
  *
  *    If any of the P_i are variable patterns, the corresponding `x_i @ P_i' is not generated
  *    and the variable constituting P_i is used instead of x_i
  *
  *  @param mapName      The name to be used for maps (either map or foreach)
  *  @param flatMapName  The name to be used for flatMaps (either flatMap or foreach)
  *  @param enums        The enumerators in the for expression
  *  @param body          The body of the for expression
  */
  def mkFor(enums: List[Tree], sugarBody: Tree): Tree = {
    val (mapName, flatMapName, body) = sugarBody match {
      case Yield(tree) => (nme.map, nme.flatMap, tree)
      case _           => (nme.foreach, nme.foreach, sugarBody)
    }

    /* make a closure pat => body.
     * The closure is assigned a transparent position with the point at pos.point and
     * the limits given by pat and body.
     */
    def makeClosure(pos: Position, pat: Tree, body: Tree): Tree = {
      def wrapped  = wrappingPos(List(pat, body))
      def splitpos = (if (pos != NoPosition) wrapped.withPoint(pos.point) else pos).makeTransparent
      matchVarPattern(pat) match {
        case Some((name, tpt)) =>
          Function(
            List(atPos(pat.pos) { ValDef(Modifiers(PARAM), name.toTermName, tpt, EmptyTree) }),
            body) setPos splitpos
        case None =>
          atPos(splitpos) {
            mkVisitor(List(CaseDef(pat, EmptyTree, body)), checkExhaustive = false)
          }
      }
    }

    /* Make an application  qual.meth(pat => body) positioned at `pos`.
     */
    def makeCombination(pos: Position, meth: TermName, qual: Tree, pat: Tree, body: Tree): Tree =
      // ForAttachment on the method selection is used to differentiate
      // result of for desugaring from a regular method call
      Apply(Select(qual, meth) setPos qual.pos,
        List(makeClosure(pos, pat, body))) setPos pos

    /* If `pat` is not yet a `Bind` wrap it in one with a fresh name */
    def makeBind(pat: Tree): Tree = pat match {
      case Bind(_, _) => pat
      case _ => Bind(freshTermName(), pat) setPos pat.pos
    }

    /* A reference to the name bound in Bind `pat`. */
    def makeValue(pat: Tree): Tree = pat match {
      case Bind(name, _) => Ident(name) setPos pat.pos.focus
    }

    /* The position of the closure that starts with generator at position `genpos`. */
    def closurePos(genpos: Position) =
      if (genpos == NoPosition) NoPosition
      else {
        val end = body.pos match {
          case NoPosition => genpos.point
          case bodypos => bodypos.end
        }
        rangePos(genpos.source, genpos.start, genpos.point, end)
      }

    enums match {
      case (t @ ValFrom(pat, rhs)) :: Nil =>
        makeCombination(closurePos(t.pos), mapName, rhs, pat, body)
      case (t @ ValFrom(pat, rhs)) :: (rest @ (ValFrom(_, _) :: _)) =>
        makeCombination(closurePos(t.pos), flatMapName, rhs, pat,
                        mkFor(rest, sugarBody))
      case (t @ ValFrom(pat, rhs)) :: Filter(test) :: rest =>
        mkFor(ValFrom(pat, makeCombination(rhs.pos union test.pos, nme.withFilter, rhs, pat.duplicate, test)).setPos(t.pos) :: rest, sugarBody)
      case (t @ ValFrom(pat, rhs)) :: rest =>
        val valeqs = rest.take(settings.maxTupleArity - 1).takeWhile { ValEq.unapply(_).nonEmpty }
        assert(!valeqs.isEmpty)
        val rest1 = rest.drop(valeqs.length)
        val pats = valeqs map { case ValEq(pat, _) => pat }
        val rhss = valeqs map { case ValEq(_, rhs) => rhs }
        val defpat1 = makeBind(pat)
        val defpats = pats map makeBind
        val pdefs = (defpats, rhss).zipped flatMap mkPatDef
        val ids = (defpat1 :: defpats) map makeValue
        val rhs1 = mkFor(
          List(ValFrom(defpat1, rhs).setPos(t.pos)),
          Yield(Block(pdefs, atPos(wrappingPos(ids)) { mkTuple(ids) }) setPos wrappingPos(pdefs)))
        val allpats = (pat :: pats) map (_.duplicate)
        val pos1 =
          if (t.pos == NoPosition) NoPosition
          else rangePos(t.pos.source, t.pos.start, t.pos.point, rhs1.pos.end)
        val vfrom1 = ValFrom(atPos(wrappingPos(allpats)) { mkTuple(allpats) }, rhs1).setPos(pos1)
        mkFor(vfrom1 :: rest1, sugarBody)
      case _ =>
        EmptyTree //may happen for erroneous input

    }
  }

  /** Create tree for pattern definition <val pat0 = rhs> */
  def mkPatDef(pat: Tree, rhs: Tree): List[ValDef] =
    mkPatDef(Modifiers(0), pat, rhs)

  /** Create tree for pattern definition <mods val pat0 = rhs> */
  def mkPatDef(mods: Modifiers, pat: Tree, rhs: Tree): List[ValDef] = matchVarPattern(pat) match {
    case Some((name, tpt)) =>
      List(atPos(pat.pos union rhs.pos) {
        ValDef(mods, name.toTermName, tpt, rhs)
      })

    case None =>
      //  in case there is exactly one variable x_1 in pattern
      //  val/var p = e  ==>  val/var x_1 = e.match (case p => (x_1))
      //
      //  in case there are zero or more than one variables in pattern
      //  val/var p = e  ==>  private synthetic val t$ = e.match (case p => (x_1, ..., x_N))
      //                  val/var x_1 = t$._1
      //                  ...
      //                  val/var x_N = t$._N

      val rhsUnchecked = mkUnchecked(rhs)

      // TODO: clean this up -- there is too much information packked into mkPatDef's `pat` argument
      // when it's a simple identifier (case Some((name, tpt)) -- above),
      // pat should have the type ascription that was specified by the user
      // however, in `case None` (here), we must be careful not to generate illegal pattern trees (such as `(a, b): Tuple2[Int, String]`)
      // i.e., this must hold: pat1 match { case Typed(expr, tp) => assert(expr.isInstanceOf[Ident]) case _ => }
      // if we encounter such an erroneous pattern, we strip off the type ascription from pat and propagate the type information to rhs
      val (pat1, rhs1) = patvarTransformer.transform(pat) match {
        // move the Typed ascription to the rhs
        case Typed(expr, tpt) if !expr.isInstanceOf[Ident] =>
          val rhsTypedUnchecked =
            if (tpt.isEmpty) rhsUnchecked
            else Typed(rhsUnchecked, tpt) setPos (rhs.pos union tpt.pos)
          (expr, rhsTypedUnchecked)
        case ok =>
          (ok, rhsUnchecked)
      }
      val vars = getVariables(pat1)
      val matchExpr = atPos((pat1.pos union rhs.pos).makeTransparent) {
        Match(
          rhs1,
          List(
            atPos(pat1.pos) {
              CaseDef(pat1, EmptyTree, mkTuple(vars map (_._1) map Ident.apply))
            }
          ))
      }
      vars match {
        case List((vname, tpt, pos)) =>
          List(atPos(pat.pos union pos union rhs.pos) {
            ValDef(mods, vname.toTermName, tpt, matchExpr)
          })
        case _ =>
          val tmp = freshTermName()
          val firstDef =
            atPos(matchExpr.pos) {
              ValDef(Modifiers(PrivateLocal | SYNTHETIC | ARTIFACT | (mods.flags & LAZY)),
                     tmp, TypeTree(), matchExpr)
            }
          var cnt = 0
          val restDefs = for ((vname, tpt, pos) <- vars) yield atPos(pos) {
            cnt += 1
            ValDef(mods, vname.toTermName, tpt, Select(Ident(tmp), newTermName("_" + cnt)))
          }
          firstDef :: restDefs
      }
  }

  /** Create tree for for-comprehension generator <val pat0 <- rhs0> */
  def mkGenerator(pos: Position, pat: Tree, valeq: Boolean, rhs: Tree): Tree = {
    val pat1 = patvarTransformer.transform(pat)
    if (valeq) ValEq(pat1, rhs).setPos(pos)
    else ValFrom(pat1, mkCheckIfRefutable(pat1, rhs)).setPos(pos)
  }

  def mkCheckIfRefutable(pat: Tree, rhs: Tree) =
    if (TreeInfo.isVarPatternDeep(pat)) rhs
    else {
      val cases = List(
        CaseDef(pat.duplicate, EmptyTree, Literal(Constant(true))),
        CaseDef(Ident(nme.WILDCARD), EmptyTree, Literal(Constant(false)))
      )
      val visitor = mkVisitor(cases, checkExhaustive = false, nme.CHECK_IF_REFUTABLE_STRING)
      atPos(rhs.pos)(Apply(Select(rhs, nme.withFilter), visitor :: Nil))
    }

  /** If tree is a variable pattern, return Some("its name and type").
   *  Otherwise return none */
  private def matchVarPattern(tree: Tree): Option[(Name, Tree)] = {
    def wildType(t: Tree): Option[Tree] = t match {
      case Ident(x) if x.toTermName == nme.WILDCARD             => Some(TypeTree())
      case Typed(Ident(x), tpt) if x.toTermName == nme.WILDCARD => Some(tpt)
      case _                                                    => None
    }
    tree match {
      case Ident(name)             => Some((name, TypeTree()))
      case Bind(name, body)        => wildType(body) map (x => (name, x))
      case Typed(Ident(name), tpt) => Some((name, tpt))
      case _                       => None
    }
  }

  /** Create visitor <x => x match cases> */
  def mkVisitor(cases: List[CaseDef], checkExhaustive: Boolean, prefix: String = "x$"): Tree = {
    val x   = freshTermName(prefix)
    val id  = Ident(x)
    val sel = if (checkExhaustive) id else mkUnchecked(id)
    Function(List(mkSyntheticParam(x)), Match(sel, cases))
  }

  /** Traverse pattern and collect all variable names with their types in buffer
   *  The variables keep their positions; whereas the pattern is converted to be
   *  synthetic for all nodes that contain a variable position.
   */
  class GetVarTraverser extends Traverser {
    val buf = new ListBuffer[(Name, Tree, Position)]

    def namePos(tree: Tree, name: Name): Position =
      if (!tree.pos.isRange || name.containsName(nme.raw.DOLLAR)) tree.pos.focus
      else {
        val start = tree.pos.start
        val end = start + name.decode.length
        rangePos(tree.pos.source, start, start, end)
      }

    override def traverse(tree: Tree): Unit = {
      def seenName(name: Name)     = buf exists (_._1 == name)
      def add(name: Name, t: Tree) = if (!seenName(name)) buf += ((name, t, namePos(tree, name)))
      val bl = buf.length

      tree match {
        case Bind(nme.WILDCARD, _)          =>
          super.traverse(tree)

        case Bind(name, Typed(tree1, tpt))  =>
          val newTree = if (TreeInfo.mayBeTypePat(tpt)) TypeTree() else tpt.duplicate
          add(name, newTree)
          traverse(tree1)

        case Bind(name, tree1)              =>
          // can assume only name range as position, as otherwise might overlap
          // with binds embedded in pattern tree1
          add(name, TypeTree())
          traverse(tree1)

        case _ =>
          super.traverse(tree)
      }
      if (buf.length > bl)
        tree setPos tree.pos.makeTransparent
    }
    def apply(tree: Tree) = {
      traverse(tree)
      buf.toList
    }
  }

  /** Returns list of all pattern variables, possibly with their types,
   *  without duplicates
   */
  private def getVariables(tree: Tree): List[(Name, Tree, Position)] =
    new GetVarTraverser apply tree

  /** Convert all occurrences of (lower-case) variables in a pattern as follows:
   *    x                  becomes      x @ _
   *    x: T               becomes      x @ (_: T)
   */
  object patvarTransformer extends Transformer {
    override def transform(tree: Tree): Tree = tree match {
      case Ident(name) if (TreeInfo.isVarPattern(tree) && name != nme.WILDCARD) =>
        atPos(tree.pos)(Bind(name, atPos(tree.pos.focus) (Ident(nme.WILDCARD))))
      case Typed(id @ Ident(name), tpt) if (TreeInfo.isVarPattern(id) && name != nme.WILDCARD) =>
        atPos(tree.pos.withPoint(id.pos.point)) {
          Bind(name, atPos(tree.pos.withStart(tree.pos.point)) {
            Typed(Ident(nme.WILDCARD), tpt)
          })
        }
      case Apply(fn @ Apply(_, _), args) =>
        treeCopy.Apply(tree, transform(fn), transformTrees(args))
      case Apply(fn, args) =>
        treeCopy.Apply(tree, fn, transformTrees(args))
      case Typed(expr, tpt) =>
        treeCopy.Typed(tree, transform(expr), tpt)
      case Bind(name, body) =>
        treeCopy.Bind(tree, name, transform(body))
      case Alternative(_) | Star(_) =>
        super.transform(tree)
      case _ =>
        tree
    }
  }

  // annotate the expression with @unchecked
  def mkUnchecked(expr: Tree): Tree = atPos(expr.pos) {
    // This can't be "Annotated(New(UncheckedClass), expr)" because annotations
    // are very picky about things and it crashes the compiler with "unexpected new".
    Annotated(New(scalaDot(tpnme.unchecked), Nil), expr)
  }

  def mkSyntheticParam(pname: TermName) =
    ValDef(Modifiers(PARAM | SYNTHETIC), pname, TypeTree(), EmptyTree)

  /** Translate names in Select/Ident nodes to type names.
   */
  def convertToTypeName(tree: Tree): Option[RefTree] = tree match {
    case Select(qual, name) => Some(Select(qual, name.toTypeName))
    case Ident(name)        => Some(Ident(name.toTypeName))
    case _                  => None
  }
}
