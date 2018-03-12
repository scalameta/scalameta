
## Expressions (`meta.Term`)

```scala
"1 + 1".parse[Term].get.structure
```

Ref             | Examples
----------------|-----------
Term.This       | `this`, `foo.this`
Term.Super      | `super`, `foo.super`, `super[bar]"`, `foo.super[bar]`
Term.Name       | `foo`
Term.Select     | `bar.foo`
Term.ApplyUnary | `!foo`, `~foo`, `-foo`, `foo`

Term                  | Examples
----------------------|------------------------------
Term.Apply            | `buzz(42)`
Term.ApplyType        | `implicitly[Ordering[Int]]`
Term.ApplyInfix       | `a + a`
Term.Assign           | `a = 1`
Term.Return           | `return bar`
Term.Throw            | `throw exception`
Term.Ascribe          | `a: Int`
Term.Annotate         | `(x: @unchecked)`
Term.Tuple            | `(1, 1)`
Term.Block            | `{ bar; buzz }`
Term.If               | `if (p) t else f`
Term.Match            | `x match { case _ => }`
Term.Try              | `try foo catch { case _ => }`
Term.TryWithHandler   | `try foo catch (h)`
Term.Function         | `(a, b) => a + b`
Term.PartialFunction  | `{ case _ =>  }`
Term.While            | `while(p){ d }`
Term.Do               | `do { d } while(p)`
Term.For              | `for{x <- xs} f(x)`
Term.ForYield         | `for{x <- xs} yield f(x)`
Term.New              | `new A`
Term.NewAnonymous     | `new A { }`
Term.Placeholder      | `_`
Term.Eta              | `foo _`
Term.Repeated         | `foo(x: _*)`
Term.Param            | `def foo<<(x: Int)>>`
Term.Interpolate      | `s"|foo ~ $bar ~ ${42}|"`
Term.Xml              | `<h1>|Hello ~ {bar} ~ {42}|</h1>`

## Types (`meta.Type`)


Ref            | Examples
--------------------------
Type.Name      | `B`
Type.Select    | `a.B`
Type.Project   | `a#B`
Type.Singleton | `t.type`

```scala
"A with B".parse[Type].get.structure
```

Type                  | Examples
----------------------|--------------------
Type.Apply            | `F[T]`
Type.ApplyInfix       | `K Map V`
Type.Function         | `A => B`
Type.ImplicitFunction | `implicit A => B`
Type.Tuple            | `(A, B)`
Type.With             | `A with B`
Type.And              | `A & B`
Type.Or               | `A \| B`
Type.Refine           | `A { def f: Int }`
Type.Existential      | `A forSome { type T }`
Type.Annotate         | `T @A`
Type.Lambda           | `[X] => (X, X)`
Type.[Method]         | ???
Type.Placeholder      | `T[_ >: Nothing <: Any]`
Type.Bounds           | `def F[T >: Nothing <: Any] = 1`
Type.ByName           | `def f(v: => T) = v`
Type.Repeated         | `def print(v: Any*): Unit`
Type.Var              | ???
Type.Param            | `trait A[X]`

[Method]: https://www.scala-lang.org/files/archive/spec/2.12/03-types.html#method-types


## Patterns (meta.Pat) and Cases (meta.Case)

```scala
"""(foo, bar)""".parse[Pat].get.structure
```

Pat                | Examples
-------------------|----------------------------
Lit                | `1`
Pat.Wildcard       | `_`
Pat.SeqWildcard    | `_*`
Pat.Var            | `foo`
Pat.Bind           | `foo @ Ext`
Pat.Alternative    | `1 | 2`
Pat.Tuple          | `(foo, bar)`
Pat.Extract        | `Ext(foo, bar)`
Pat.ExtractInfix   | `foo Ext bar`
Pat.Interpolate    | `r"example (.+)$foo"`
Pat.Xml            | `<h1>Hello, World!</h1>`
Pat.Typed          | `foo: Int`
Term.Name          | `foo`
Term.Select        | `foo.bar`


```scala
"""case a => ()""".parse[Case].get.structure
```

Case               | Examples
-------------------|----------------------------
Case               | `p"case $pat if $expropt => $expr"`

## Name (`meta.Name`)

`meta.name.Anonymous()`

```scala
"class B { }".parse[Source].get.structure

Defn.Class(
  Nil,
  Type.Name("B"),
  Nil,
  Ctor.Primary(
    Nil,
    Name.Anonymous(),
    Nil
  ),
  Template(
    Nil,
    Nil,
    Self(Name.Anonymous(), None),
    Nil
  )
)
```

`meta.name.Indeterminate()`

Is it a type, or is it a term? Indeterminate.

```scala
"import a.b".parse[Source].get.structure

Import(List(Importer(Term.Name("a"), List(Importee.Name(Indeterminate.Name("b"))))))
```

## Literals (`meta.Lit`)

```scala
"null".parse[Term].get.structure
```

Lit      |
---------|-------
Null     |  null     
Boolean  |  true     
Unit     |  ()       
Int      |  1        
Double   |  1.0      
Float    |  1.0F     
Long     |  1L       
Byte[1]  |  1Z       
Short[1] |  1S       
Char     |  'a'      
Symbol   |  'a       
String   |  "A"      

[1]: https://github.com/scalameta/scalameta/issues/1324
