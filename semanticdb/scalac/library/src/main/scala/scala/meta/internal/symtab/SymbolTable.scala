package scala.meta.internal.symtab

import scala.meta.internal.semanticdb.Scala.{Descriptor => d, _}
import scala.meta.internal.semanticdb._

import scala.annotation.tailrec
import scala.collection.mutable

trait SymbolTable {

  /** Returns the SymbolInformation for the given symbol, or None if the symbol is missing. */
  def info(symbol: String): Option[SymbolInformation]

  /**
   * Returns all same-owner overloads of `symbol` (including `symbol` itself) in declaration order
   * (the disambiguator sequence: the base overload, then `+1`, `+2`, ...), or `Nil` when the
   * overload set cannot be determined from this symbol table.
   *
   * At a resolved call site scalac picks a single overload, so the occurrence points to one
   * disambiguated symbol (e.g. `A.foo(+1).`); this recovers the full group declared alongside it
   * (`A.foo().`, `A.foo(+1).`). See https://github.com/scalameta/scalameta/issues/1298.
   *
   * A determined result always contains at least `symbol`, so `Nil` unambiguously means "could not
   * inspect" — never "no overloads". `Nil` covers the empty symbol, a local or non-method symbol,
   * and a method whose declaring owner is unavailable here or does not declare it. For an
   * unresolved multi-symbol, the members are returned.
   *
   * Scope: only overloads declared in the *same owner* are returned. Overloads inherited from, or
   * split across, supertypes are intentionally excluded — a classpath symbol table cannot resolve
   * them safely: `overriddenSymbols` is `Nil` outside the compiler plugin, so override and overload
   * are indistinguishable when walking parents, and the full receiver-type candidate set further
   * needs the call site's static receiver type, which a symbol alone does not carry.
   */
  def overloads(symbol: String): List[String] =
    if (symbol.isMulti) symbol.asMulti
    else if (symbol.isGlobal) {
      val (desc, owner) = DescriptorParser(symbol)
      desc match {
        case d.Method(name, _) =>
          // keep `owner`'s same-name methods only if `symbol` is actually one of them, so a symbol
          // that merely parses like a method of `owner` (but isn't declared there) isn't fabricated
          val sameName = membersNamed(owner, name)
          if (sameName.contains(symbol)) sameName else Nil
        case _ => Nil
      }
    } else Nil

  /**
   * Returns all overloads of `name` visible on type `tpe` — its own declarations plus those
   * inherited across its supertypes — override-deduplicated, most-derived first. `Nil` if `tpe` is
   * not a resolvable class-like symbol or has no such method.
   *
   * This is the hierarchy-aware companion to the single-argument `overloads`: a resolved method
   * symbol only exposes its own owner's overloads, but the complete candidate set at a call site
   * spans the receiver type's supertypes (e.g. `trait T { def foo(x: A) }` and
   * `class C extends T { def foo(x: B) }` — both are candidates on `C`). `tpe` is the receiver type
   * symbol (e.g. `C#`), which the caller supplies; see
   * https://github.com/scalameta/scalameta/issues/1298.
   *
   * Dedup is by erased parameter signature: an override has the same erased signature as the method
   * it overrides, so the most-derived one is kept while genuine overloads (different signatures)
   * all survive. Parent type arguments are substituted before erasing, so a generic override
   * deduplicates correctly (`trait Base[A] { def foo(a: A) }`, `class Sub extends Base[String] {
   * override def foo(a: String) }` collapse to one `foo`). A method whose parameter types cannot be
   * resolved is conservatively kept rather than risk a wrong merge.
   *
   * Constructors are not inherited, so for `name == "<init>"` only `tpe`'s own constructors are
   * returned, not its parents'.
   *
   * Caveats: the walk is a simple most-derived-first parent traversal, not full Scala linearization
   * (enough for override dedup, not for exact resolution order); erasure is symbol-level; and
   * accessibility/applicability is not applied — final overload resolution is the compiler's job.
   */
  def overloads(tpe: String, name: String): List[String] = {
    // constructors are not inherited, so do not walk parents for the `<init>` name
    val bases = if (name == "<init>") List(tpe -> Map.empty[String, String]) else linearization(tpe)
    val seenSignatures = mutable.Set.empty[List[String]]
    bases.flatMap { case (owner, env) =>
      // an unresolved erased signature (None) keeps the method rather than risk a wrong merge
      membersNamed(owner, name).filter(erasedSignature(_, env).forall(seenSignatures.add))
    }
  }

  /**
   * Tests whether `child` is an erased subtype of `parent`.
   *
   * "Erased" means only symbols are compared and type arguments are ignored — the semantics of
   * `java.lang.Class.isAssignableFrom`, which is what the motivating Scalafix rules need (see
   * scalameta/scalameta#1213). Unlike that JVM-reflection approach it reads the compiler-emitted
   * SemanticDB rather than loading the analyzed program's classes, so it also covers targets whose
   * classes are not JVM-loadable (e.g. Scala.js/Native projects); the query itself runs on the JVM.
   * For example, a rule banning public APIs that expose `Throwable` subtypes
   * (scalacenter/scalafix#531) checks the symbol from a method's return-type `TypeRef` with
   * `symtab.isSubtypeOf(returnTypeSymbol, "java/lang/Throwable#")`.
   *
   * It is reflexive (by symbol equality, so even for symbols not resolvable via [[info]]) and walks
   * `ClassSignature.parents` transitively, following type aliases through
   * `TypeSignature.upperBound` (e.g. the scala-library parent `scala/package.RuntimeException#`
   * resolves to `java/lang/RuntimeException#`) and unwrapping
   * structural/annotated/existential/universal type wrappers. A `parent` merely named in the
   * ancestor chain matches even when its own `SymbolInformation` is missing, but an unresolvable
   * intermediate link stops the walk there, yielding `false` rather than throwing; the traversal is
   * cycle-safe.
   */
  def isSubtypeOf(child: String, parent: String): Boolean = {
    val seen = mutable.Set.empty[String]
    def loop(sym: String): Boolean = sym == parent || seen.add(sym) && info(sym)
      .exists(parentSymbols(_).exists(loop))
    loop(child)
  }

  // Parent (symbol, type-arguments) pairs from a class signature, or the alias target for a type.
  private def parentRefs(sinfo: SymbolInformation): Iterator[(String, Seq[Type])] =
    sinfo.signature match {
      case cs: ClassSignature => typeRefsFlatten(cs.parents)
      case ts: TypeSignature => typeRefs(ts.upperBound) // follow type-alias upper bound
      case _ => Iterator.empty
    }

  @tailrec
  private def typeRefs(tpe: Type): Iterator[(String, Seq[Type])] = tpe match {
    case TypeRef(_, symbol, args) => Iterator(symbol -> args)
    case WithType(types) => typeRefsFlatten(types)
    case IntersectionType(types) => typeRefsFlatten(types)
    // unwrap type wrappers to the underlying type(s); e.g. a refined parent is encoded as
    // StructuralType(WithType(...)) (see scalacp/TypeOps.scala).
    case StructuralType(t, _) => typeRefs(t)
    case AnnotatedType(_, t) => typeRefs(t)
    case ExistentialType(t, _) => typeRefs(t)
    case UniversalType(_, t) => typeRefs(t)
    case _ => Iterator.empty
  }

  private def typeRefsFlatten(types: Iterable[Type]): Iterator[(String, Seq[Type])] = types.iterator
    .flatMap(typeRefs)

  // symbol-only views, used where type arguments aren't needed (e.g. `isSubtypeOf`)
  private def parentSymbols(sinfo: SymbolInformation): Iterator[String] = parentRefs(sinfo).map(_._1)
  private def typeSymbols(tpe: Type): Iterator[String] = typeRefs(tpe).map(_._1)

  // `tpe` plus its transitive parents, most-derived first and cycle-safe, each paired with the
  // substitution of its type parameters to erased symbols (so generic overrides compare correctly).
  // A type always precedes its own ancestors, so dedup-by-first-seen keeps the most-derived method.
  private def linearization(tpe: String): List[(String, Map[String, String])] = {
    val seen = mutable.LinkedHashMap.empty[String, Map[String, String]]
    def loop(sym: String, env: Map[String, String]): Unit = if (!seen.contains(sym)) {
      seen(sym) = env
      info(sym).foreach(parentRefs(_).foreach { case (parent, args) =>
        loop(parent, bindTypeParams(parent, args, env))
      })
    }
    loop(tpe, Map.empty)
    seen.toList
  }

  // Map `owner`'s type parameters to the erased symbols of `args` (resolved through the caller's
  // `env`); unbound when args are missing (raw type), so such parameters erase to their upper bound.
  private def bindTypeParams(
      owner: String,
      args: Seq[Type],
      env: Map[String, String],
  ): Map[String, String] =
    if (args.isEmpty) Map.empty
    else info(owner).map(_.signature) match {
      case Some(c: ClassSignature) => c.typeParameters.symbols.iterator.zip(args.iterator)
          .flatMap { case (param, arg) => headSymbol(arg, env).map(param -> _) }.toMap
      case _ => Map.empty
    }

  private def membersNamed(owner: String, name: String): List[String] =
    info(owner).map(_.signature) match {
      case Some(c: ClassSignature) => c.declarations.symbols.filter(_.desc match {
          case d.Method(`name`, _) => true
          case _ => false
        })
      case _ => Nil
    }

  // The erased parameter-type symbols of `method` under substitution `env`, or None when it is not a
  // resolvable method or a parameter type can't be reduced (so callers don't merge what they can't
  // compare).
  private def erasedSignature(method: String, env: Map[String, String]): Option[List[String]] =
    info(method).map(_.signature) match {
      case Some(m: MethodSignature) =>
        val types = m.parameterLists.iterator.flatMap(paramInfos).map(_.signature match {
          case v: ValueSignature => headSymbol(v.tpe, env)
          case _ => None
        }).toList
        if (types.forall(_.isDefined)) Some(types.flatten) else None
      case _ => None
    }

  private def paramInfos(scope: Scope): Iterator[SymbolInformation] =
    if (scope.symlinks.nonEmpty) scope.symlinks.iterator.flatMap(info) else scope.hardlinks.iterator

  // Reduce a type to its erased head symbol: unwrap by-name/repeated wrappers, then resolve a type
  // parameter through `env` (substitution) or, failing that, follow its alias/upper bound.
  private def headSymbol(tpe: Type, env: Map[String, String]): Option[String] = {
    // resolve a type parameter through `env` (substitution), else follow its alias/upper bound;
    // `seen` guards against alias/type-parameter cycles.
    @tailrec
    def resolve(sym: String, seen: Set[String]): String = env.get(sym) match {
      case Some(bound) => bound
      case None => info(sym).map(_.signature) match {
          case Some(ts: TypeSignature) => typeSymbols(ts.upperBound).toList.headOption match {
              case Some(next) if !seen(next) => resolve(next, seen + next)
              case _ => sym
            }
          case _ => sym
        }
    }
    def head(t: Type): Option[String] = t match {
      case ByNameType(u) => head(u)
      case RepeatedType(u) => head(u)
      case _ => typeSymbols(t).toList.headOption
    }
    head(tpe).map(resolve(_, Set.empty))
  }

}
