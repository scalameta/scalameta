package scala.meta.internal.symtab

import org.scalameta.collections.Conversions._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d, _}
import scala.meta.internal.semanticdb._

import scala.annotation.tailrec
import scala.collection.mutable
import scala.reflect.ClassTag

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
   * inherited across its supertypes — override-deduplicated, in linearization order. `Nil` if `tpe`
   * is not a resolvable class-like symbol or has no such method.
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
   * Caveats: erasure is approximate, and accessibility/applicability is not applied — final
   * overload resolution is the compiler's job.
   *
   * To tell whether a parent was missing from this symbol table, use [[resolveOverloads]].
   */
  def overloads(tpe: String, name: String): List[String] = resolveOverloads(tpe, name).symbols

  /**
   * Returns the result of `overloads(tpe, name)` together with the types in the walk whose `info`
   * is missing. The result can lack the overloads that a missing type declares or inherits.
   */
  def resolveOverloads(tpe: String, name: String): SymbolTable.Overloads = {
    // constructors are not inherited, so do not walk parents for the `<init>` name
    val bases = if (name == "<init>") List(tpe -> Map.empty[String, String]) else linearization(tpe)
    val seenSignatures = mutable.Set.empty[List[String]]
    val res = List.newBuilder[String]
    val unres = List.newBuilder[String]
    bases.foreach { case (owner, env) =>
      if (info(owner).isEmpty) unres += owner
      // an unresolved erased signature (None) keeps the method rather than risk a wrong merge;
      // methods of one owner cannot override each other, so we add their signatures only after
      val ownerSignatures = mutable.ListBuffer.empty[List[String]]
      membersNamed(owner, name).foreach(m =>
        erasedSignature(m, env) match {
          case Some(sig) =>
            if (!seenSignatures.contains(sig)) res += m
            ownerSignatures += sig
          case None => res += m
        },
      )
      seenSignatures ++= ownerSignatures
    }
    SymbolTable.Overloads(res.result(), unres.result())
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
   *
   * To tell a genuine `false` from an unresolvable link, use [[subtypeOf]].
   */
  def isSubtypeOf(child: String, parent: String): Boolean = subtypeOf(child, parent).contains(true)

  /**
   * Like [[isSubtypeOf]], but returns `None` instead of `false` if the walk reached a type whose
   * `info` is missing.
   */
  def subtypeOf(child: String, parent: String): Option[Boolean] = {
    val seen = mutable.Set.empty[String]
    def loop(sym: String): Option[Boolean] =
      if (sym == parent) Some(true)
      else if (!seen.add(sym)) Some(false) // its first visit already counted
      else info(sym) match {
        case None => None
        case Some(sinfo) =>
          val parents = parentSymbols(sinfo)
          var res: Option[Boolean] = Some(false)
          while (parents.hasNext && !res.contains(true)) loop(parents.next()) match {
            case Some(false) =>
            case r => res = r
          }
          res
      }
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
    case ByNameType(t) => typeRefs(t)
    case RepeatedType(t) => typeRefs(t)
    case _ => Iterator.empty
  }

  private def typeRefsFlatten(types: Iterable[Type]): Iterator[(String, Seq[Type])] = types.iterator
    .flatMap(typeRefs)

  // symbol-only views, used where type arguments aren't needed (e.g. `isSubtypeOf`)
  private def parentSymbols(sinfo: SymbolInformation): Iterator[String] = parentRefs(sinfo).map(_._1)

  // `tpe` plus its transitive parents in Scala linearization order, cycle-safe, each paired with the
  // substitution of its type parameters to erased symbols (so generic overrides compare correctly).
  // A type always precedes its own ancestors, so dedup-by-first-seen keeps the most-derived method.
  private def linearization(tpe: String): List[(String, Map[String, String])] = {
    val res = mutable.ListBuffer.empty[String]
    val visited = mutable.Set.empty[String]
    /* a post-order walk over the parents in declaration order visits the linearization in reverse;
     * we mark a type on entry, so a cycle terminates */
    def loop(sym: String): Unit = if (visited.add(sym)) {
      info(sym).foreach(parentSymbols(_).foreach(loop))
      res.prepend(sym)
    }
    loop(tpe)
    val envs = mutable.HashMap(tpe -> Map.empty[String, String])
    res.toList.map { sym =>
      val env = envs.getOrElse(sym, Map.empty)
      // a type follows all its subtypes, so its env is set before we reach it
      info(sym).foreach(parentRefs(_).foreach { case (parent, args) =>
        if (!envs.contains(parent)) envs(parent) = bindTypeParams(parent, args, env)
      })
      sym -> env
    }
  }

  // Map `owner`'s type parameters to the erased symbols of `args` (resolved through the caller's
  // `env`); unbound when args are missing (raw type), so such parameters erase to their upper bound.
  private def bindTypeParams(
      owner: String,
      args: Seq[Type],
      env: Map[String, String],
  ): Map[String, String] =
    if (args.isEmpty) Map.empty
    else owner.signature[ClassSignature].map(c =>
      c.typeParameters.symbols.iterator.zip(args.iterator).flatMap { case (param, arg) =>
        erasure(arg, env).map(param -> _)
      }.toMap,
    ).getOrElse(Map.empty)

  private def membersNamed(owner: String, name: String): List[String] = owner
    .signature[ClassSignature].map(_.declarations.symbols.filter(_.desc match {
      case d.Method(`name`, _) => true
      case _ => false
    })).getOrElse(Nil)

  // The erased parameter-type symbols of `method` under substitution `env`, or None when it is not a
  // resolvable method or a parameter type can't be reduced (so callers don't merge what they can't
  // compare).
  private def erasedSignature(method: String, env: Map[String, String]): Option[List[String]] =
    method.signature[MethodSignature].flatMap { m =>
      val types = m.parameterLists.iterator.flatMap(paramInfos).map(_.signature match {
        case v: ValueSignature => erasure(v.tpe, env)
        case _ => None
      }).toList
      if (types.forall(_.isDefined)) Some(types.flatten) else None
    }

  private def paramInfos(scope: Scope): Iterator[SymbolInformation] =
    if (scope.symlinks.nonEmpty) scope.symlinks.iterator.flatMap(info) else scope.hardlinks.iterator

  /* Erase a type to its head symbol, and resolve a type parameter through `env`, else through its
   * alias/upper bound. As on the JVM, a by-name type erases to `Function0`, and an array keeps its
   * element. A repeated type stays apart from a plain one. */
  private def erasure(tpe: Type, env: Map[String, String]): Option[String] = {
    val seen = mutable.Set.empty[String] // guards against alias/type-parameter cycles
    def loop(t: Type): Option[String] = t match {
      case _: ByNameType => Some("scala/Function0#")
      case RepeatedType(elem) => loop(elem).map(_ + "*")
      case _ => typeRefs(t).nextOption().flatMap {
          case ("scala/Array#", Seq(elem)) => loop(elem).map(e => s"scala/Array#[$e]")
          case (sym, _) => env.get(sym).orElse(sym.signature[TypeSignature] match {
              case Some(ts) if seen.add(sym) => loop(ts.upperBound).orElse(Some(sym))
              case _ => Some(sym)
            })
        }
    }
    loop(tpe)
  }

  implicit class XtensionOptionSymbolInformation(private val obj: Option[SymbolInformation]) {
    def signature[T <: Signature](implicit classTag: ClassTag[T]): Option[T] = obj.map(_.signature)
      .flatMap(classTag.unapply)
  }

  implicit class XtensionSymbol(private val symbol: String) {
    def signature[T <: Signature: ClassTag]: Option[T] = info(symbol).signature[T]
  }

}

object SymbolTable {

  /**
   * @param symbols
   *   the overloads, as `overloads(tpe, name)` returns them
   * @param unresolved
   *   the types in the walk whose `info` is missing; the result is complete only if this is empty
   */
  final case class Overloads(symbols: List[String], unresolved: List[String])

}
