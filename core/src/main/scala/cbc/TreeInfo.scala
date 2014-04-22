/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

package cbc
import cbc.Flags._
import Trees._, Positions._, Names._, StdNames._

/** This class ...
 *
 *  @author Martin Odersky
 *  @version 1.0
 */
object TreeInfo {
  def isEarlyDef(tree: Tree) = tree match {
    case TypeDef(mods, _, _, _) => mods hasFlag PRESUPER
    case ValDef(mods, _, _, _) => mods hasFlag PRESUPER
    case _ => false
  }

  def isEarlyValDef(tree: Tree) = tree match {
    case ValDef(mods, _, _, _) => mods hasFlag PRESUPER
    case _ => false
  }

  /** Translates an Assign(_, _) node to AssignOrNamedArg(_, _) if
   *  the lhs is a simple ident. Otherwise returns unchanged.
   */
  def assignmentToMaybeNamedArg(tree: Tree) = tree match {
    case t @ Assign(id: Ident, rhs) => atPos(t.pos)(AssignOrNamedArg(id, rhs))
    case t                          => t
  }

  /** Is name a left-associative operator? */
  def isLeftAssoc(operator: Name) = operator.nonEmpty && (operator.endChar != ':')

  /** Is tpt a vararg type of the form T* ? */
  def isRepeatedParamType(tpt: Tree) = tpt match {
    case AppliedTypeTree(Select(_, tpnme.REPEATED_PARAM_CLASS_NAME), _)      => true
    case AppliedTypeTree(Select(_, tpnme.JAVA_REPEATED_PARAM_CLASS_NAME), _) => true
    case _                                                                   => false
  }

  /** Is tpt a by-name parameter type of the form => T? */
  def isByNameParamType(tpt: Tree) = tpt match {
    case AppliedTypeTree(Select(_, tpnme.BYNAME_PARAM_CLASS_NAME), _) => true
    case _                                                            => false
  }

  /**
   * Does this tree represent an irrefutable pattern match
   * in the position `for { <tree> <- expr }` based only
   * on information at the `parser` phase? To qualify, there
   * may be no subtree that will be interpreted as a
   * Stable Identifier Pattern, nor any type tests, even
   * on TupleN. See SI-6968.
   *
   * For instance:
   *
   * {{{
   * (foo @ (bar @ _)) = 0
   * }}}
   *
   * is a not a variable pattern; if only binds names.
   *
   * The following are not variable patterns.
   *
   * {{{
   *   `bar`
   *   Bar
   *   (a, b)
   *   _: T
   * }}}
   *
   * If the pattern is a simple identifier, it is always
   * a variable pattern. For example, the following
   * introduce new bindings:
   *
   * {{{
   * for { X <- xs } yield X
   * for { `backquoted` <- xs } yield `backquoted`
   * }}}
   *
   * Note that this differs from a case clause:
   *
   * {{{
   *   object X
   *   scrut match {
   *      case X =>  // case _ if scrut == X
   *   }
   * }}}
   *
   * Background: [[https://groups.google.com/d/msg/scala-internals/qwa_XOw_7Ks/IktkeTBYqg0J]]
   *
   */
  def isVarPatternDeep(tree: Tree): Boolean = {
    def isVarPatternDeep0(tree: Tree): Boolean = {
      tree match {
        case Bind(name, pat)  => isVarPatternDeep0(pat)
        case Ident(name)      => isVarPattern(tree)
        case _                => false
      }
    }
    tree match {
      case Ident(name) => true
      case _           => isVarPatternDeep0(tree)
    }
  }

  /** Is tree a variable pattern? */
  def isVarPattern(pat: Tree): Boolean = pat match {
    case x: Ident           => !x.isBackquoted && nme.isVariableName(x.name)
    case _                  => false
  }

  /** Is tree legal as a member definition of an interface?
   */
  def isInterfaceMember(tree: Tree): Boolean = tree match {
    case EmptyTree                     => true
    case Import(_, _)                  => true
    case TypeDef(_, _, _, _)           => true
    case DefDef(mods, _, _, _, _, __)  => mods.isDeferred
    case ValDef(mods, _, _, _)         => mods.isDeferred
    case _ => false
  }

  /** Applications in Scala can have one of the following shapes:
   *
   *    1) naked core: Ident(_) or Select(_, _) or basically anything else
   *    2) naked core with targs: TypeApply(core, targs) or AppliedTypeTree(core, targs)
   *    3) apply or several applies wrapping a core: Apply(core, _), or Apply(Apply(core, _), _), etc
   *
   *  This class provides different ways to decompose applications and simplifies their analysis.
   *
   *  ***Examples***
   *  (TypeApply in the examples can be replaced with AppliedTypeTree)
   *
   *    Ident(foo):
   *      * callee = Ident(foo)
   *      * core = Ident(foo)
   *      * targs = Nil
   *      * argss = Nil
   *
   *    TypeApply(foo, List(targ1, targ2...))
   *      * callee = TypeApply(foo, List(targ1, targ2...))
   *      * core = foo
   *      * targs = List(targ1, targ2...)
   *      * argss = Nil
   *
   *    Apply(foo, List(arg1, arg2...))
   *      * callee = foo
   *      * core = foo
   *      * targs = Nil
   *      * argss = List(List(arg1, arg2...))
   *
   *    Apply(Apply(foo, List(arg21, arg22, ...)), List(arg11, arg12...))
   *      * callee = foo
   *      * core = foo
   *      * targs = Nil
   *      * argss = List(List(arg11, arg12...), List(arg21, arg22, ...))
   *
   *    Apply(Apply(TypeApply(foo, List(targs1, targs2, ...)), List(arg21, arg22, ...)), List(arg11, arg12...))
   *      * callee = TypeApply(foo, List(targs1, targs2, ...))
   *      * core = foo
   *      * targs = Nil
   *      * argss = List(List(arg11, arg12...), List(arg21, arg22, ...))
   */
  class Applied(val tree: Tree) {
    /** The tree stripped of the possibly nested applications.
     *  The original tree if it's not an application.
     */
    def callee: Tree = {
      def loop(tree: Tree): Tree = tree match {
        case Apply(fn, _) => loop(fn)
        case tree         => tree
      }
      loop(tree)
    }

    /** The `callee` unwrapped from type applications.
     *  The original `callee` if it's not a type application.
     */
    def core: Tree = callee match {
      case TypeApply(fn, _)       => fn
      case AppliedTypeTree(fn, _) => fn
      case tree                   => tree
    }

    /** The type arguments of the `callee`.
     *  `Nil` if the `callee` is not a type application.
     */
    def targs: List[Tree] = callee match {
      case TypeApply(_, args)       => args
      case AppliedTypeTree(_, args) => args
      case _                        => Nil
    }

    /** (Possibly multiple lists of) value arguments of an application.
     *  `Nil` if the `callee` is not an application.
     */
    def argss: List[List[Tree]] = {
      def loop(tree: Tree): List[List[Tree]] = tree match {
        case Apply(fn, args) => loop(fn) :+ args
        case _               => Nil
      }
      loop(tree)
    }
  }

  /** Returns a wrapper that knows how to destructure and analyze applications.
   */
  def dissectApplied(tree: Tree) = new Applied(tree)

  /** Destructures applications into important subparts described in `Applied` class,
   *  namely into: core, targs and argss (in the specified order).
   *
   *  Trees which are not applications are also accepted. Their callee and core will
   *  be equal to the input, while targs and argss will be Nil.
   *
   *  The provided extractors don't expose all the API of the `Applied` class.
   *  For advanced use, call `dissectApplied` explicitly and use its methods instead of pattern matching.
   */
  object Applied {
    def apply(tree: Tree): Applied = new Applied(tree)

    def unapply(applied: Applied): Option[(Tree, List[Tree], List[List[Tree]])] =
      Some((applied.core, applied.targs, applied.argss))

    def unapply(tree: Tree): Option[(Tree, List[Tree], List[List[Tree]])] =
      unapply(dissectApplied(tree))
  }

  /** can this type be a type pattern */
  def mayBeTypePat(tree: Tree): Boolean = tree match {
    case CompoundTypeTree(Template(tps, _, Nil)) => tps exists mayBeTypePat
    case Annotated(_, tp)                        => mayBeTypePat(tp)
    case AppliedTypeTree(constr, args)           => mayBeTypePat(constr) || args.exists(_.isInstanceOf[Bind])
    case SelectFromTypeTree(tp, _)               => mayBeTypePat(tp)
    case _                                       => false
  }
}
