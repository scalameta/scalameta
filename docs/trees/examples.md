# Tree Examples

This document lists examples of how Scala source code maps to Scalameta tree nodes.

## Top level (`meta.Source`, `meta.Pkg`, `meta.Pkg.Object`)

```scala
"package a".parse[Source].get.structure

Source(stats = List(Pkg(ref = Term.Name("a"), stats = Nil)))
```

## Declarations (`meta.Decl`)

```scala
"val a: Int".parse[Stat].get.structure

Decl.Val(
  mods = Nil,
  pats = List(Pat.Var(name = Term.Name("a"))),
  decltpe = Type.Name("Int")
)
```

| Decl        | Examples     |
| ----------- | ------------ |
| `Decl.Val`  | `val a: Int` |
| `Decl.Var`  | `var a: Int` |
| `Decl.Def`  | `def f: Int` |
| `Decl.Type` | `type T`     |

## Definitions (`meta.Defn`)

```scala
"val a = 1".parse[Stat].get.structure

Defn.Val(
  mods = Nil,
  pats = List(Pat.Var(name = Term.Name("a"))),
  decltpe = None,
  rhs = Lit.Int(value = 1)
)
```

| Defn          | Examples             |
| ------------- | -------------------- |
| `Defn.Val`    | `val a = 1`          |
| `Defn.Var`    | `var a = 1`          |
| `Defn.Def`    | `def f = 1`          |
| `Defn.Macro`  | `def f = macro impl` |
| `Defn.Type`   | `type T = Int`       |
| `Defn.Class`  | `class A`            |
| `Defn.Trait`  | `trait A`            |
| `Defn.Object` | `object A`           |

## Terms (`meta.Term`)

```scala
"1 + 1".parse[Term].get.structure

Term.ApplyInfix(
  lhs = Lit.Int(value = 1),
  op = Term.Name("+"),
  targs = Nil,
  args = List(Lit.Int(value = 1))
)
```

| Ref               | Examples                                     |
| ----------------- | -------------------------------------------- |
| `Term.This`       | `this`, `a.this`                             |
| `Term.Super`      | `super`, `a.super`, `super[a]`, `a.super[b]` |
| `Term.Name`       | `a`                                          |
| `Term.Select`     | `a.b`                                        |
| `Term.ApplyUnary` | `!a`, `~a`, `-a`, `a`                        |

| Term                   | Examples                    |
| ---------------------- | --------------------------- |
| `Term.Apply`           | `f(42)`                     |
| `Term.ApplyType`       | `implicitly[Ordering[Int]]` |
| `Term.ApplyInfix`      | `a + a`                     |
| `Term.Assign`          | `a = 1`                     |
| `Term.Return`          | `return a`                  |
| `Term.Throw`           | `throw e`                   |
| `Term.Ascribe`         | `a: Int`                    |
| `Term.Annotate`        | `(x: @annot)`               |
| `Term.Tuple`           | `(1, 1)`                    |
| `Term.Block`           | `{ f1(); f2() }`            |
| `Term.If`              | `if (p) t else f`           |
| `Term.Match`           | `x match { case _ => }`     |
| `Term.Try`             | `try f catch { case _ => }` |
| `Term.TryWithHandler`  | `try f catch (h)`           |
| `Term.Function`        | `(a, b) => a + b`           |
| `Term.PartialFunction` | `{ case _ => }`             |
| `Term.While`           | `while(p){ f() }`           |
| `Term.Do`              | `do { f() } while(p)`       |
| `Term.For`             | `for{x <- xs} f(x)`         |
| `Term.ForYield`        | `for{x <- xs} yield f(x)`   |
| `Term.New`             | `new A`                     |
| `Term.NewAnonymous`    | `new A { }`                 |
| `Term.Placeholder`     | `_`                         |
| `Term.Eta`             | `f _`                       |
| `Term.Repeated`        | `f(x: _*)`                  |
| `Term.Param`           | `x: Int` in `def f(x: Int)` |
| `Term.Interpolate`     | `s"Hello $name"`            |
| `Term.Xml`             | `<h1>Hello {name}</h1>`     |

## Types (`meta.Type`)

```scala
"A with B".parse[Type].get.structure

Type.With(lhs = Type.Name("A"), rhs = Type.Name("B"))
```

| Ref              | Examples |
| ---------------- | -------- |
| `Type.Name`      | `B`      |
| `Type.Select`    | `a.B`    |
| `Type.Project`   | `a#B`    |
| `Type.Singleton` | `a.type` |

| Type                    | Examples                                                  |
| ----------------------- | --------------------------------------------------------- |
| `Type.Apply`            | `F[T]`                                                    |
| `Type.ApplyInfix`       | `K Map V`                                                 |
| `Type.Function`         | `A => B`                                                  |
| `Type.ImplicitFunction` | `implicit A => B`                                         |
| `Type.Tuple`            | `(A, B)`                                                  |
| `Type.With`             | `A with B`                                                |
| `Type.And`              | `A & B`                                                   |
| `Type.Or`               | <code> A &#124; B </code>                                 |
| `Type.Refine`           | `A { def f: Int }`                                        |
| `Type.Existential`      | `A forSome { type T }`                                    |
| `Type.Annotate`         | `T @annot`                                                |
| `Type.Lambda`           | `[T] => (T, T)` (only for supported dialects)             |
| `Type.Method`           | `(x: T): T` (only for supported dialects)                 |
| `Type.Placeholder`      | `_` in `T[_]`                                             |
| `Type.Bounds`           | `T >: Lower <: Upper` in `def F[T >: Lower <: Upper] = 1` |
| `Type.ByName`           | `=>T` in `def f(x: => T) = x`                             |
| `Type.Repeated`         | `T*` in `def f(x: T*): Unit`                              |
| `Type.Var`              | `t` in `case _: List[t] =>`                               |
| `Type.Param`            | `X` in `trait A[X]`                                       |

## Patterns (meta.Pat) and Cases (meta.Case)

```scala
"_: A | _: B".parse[Pat].get.structure

Pat.Alternative(
  Pat.Typed(Pat.Wildcard(), Type.Name("A")),
  Pat.Typed(Pat.Wildcard(), Type.Name("B"))
)
```

| Pat                | Examples                        |
| ------------------ | ------------------------------- |
| `Lit`              | `"literal"`                     |
| `Pat.Wildcard`     | `_`                             |
| `Pat.SeqWildcard`  | `_*` in `case List(xs @ _*) =>` |
| `Pat.Var`          | `a` in `case a =>`              |
| `Pat.Bind`         | `a @ A()`                       |
| `Pat.Alternative`  | <code>1 &#124; 2 </code>        |
| `Pat.Tuple`        | `(a, b)`                        |
| `Pat.Extract`      | `A(a, b)`                       |
| `Pat.ExtractInfix` | `a E b`                         |
| `Pat.Interpolate`  | `r"Hello (.+)$name"`            |
| `Pat.Xml`          | `<h1>Hello, World!</h1>`        |
| `Pat.Typed`        | `a: Int`                        |
| `Term.Select`      | `a.b`                           |

```scala
"case a => ()".parse[Case].get.structure

Case(Pat.Var(Term.Name("a")), None, Lit.Unit(()))
```

| Case   | Examples             |
| ------ | -------------------- |
| `Case` | `case a if p => f()` |

## Name (`meta.Name`)

`meta.Name.Anonymous()`

```scala
"class B { }".parse[Source].get.structure

Source(
  stats = List(
    Defn.Class(
      mods = Nil,
      name = Type.Name("B"),
      tparams = Nil,
      ctor = Ctor.Primary(
        mods = Nil,
        name = Name.Anonymous(),
        paramss = Nil
      ),
      templ = Template(
        early = Nil,
        inits = Nil,
        self = Self(
          name = Name.Anonymous(),
          decltpe = None
        ),
        stats = Nil
      )
    )
  )
)
```

`meta.name.Indeterminate()`

Is it a type, or is it a term? Indeterminate.

```scala
"import a.b".parse[Source].get.structure

Source(
  stats = List(
    Import(
      importers = List(
        Importer(
          ref = Term.Name("a"),
          importees = List(
            Importee.Name(
              name = Name.Indeterminate(
                value = b
              )
            )
          )
        )
      )
    )
  )
)
```

## Literals (`meta.Lit`)

```scala
"null".parse[Term].get.structure

Lit.Null()
```

| Lit       | Example               |
| --------- | --------------------- |
| `Null`    | `null`                |
| `Boolean` | `true`                |
| `Unit`    | `()`                  |
| `Int`     | `1`                   |
| `Double`  | `1.0`                 |
| `Float`   | `1.0F`                |
| `Long`    | `1L`                  |
| `Byte`    | (no available syntax) |
| `Short`   | (no available syntax) |
| `Char`    | `'a'`                 |
| `Symbol`  | `'a`                  |
| `String`  | `"A"`                 |
