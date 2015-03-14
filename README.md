### Scalahost

Scalahost provides implementations of scalameta/scalameta [contexts](https://github.com/scalameta/scalameta/blob/master/scalameta/src/main/scala/scala/meta/semantic/Context.scala) for compile-time and runtime metaprogramming with [scala.meta](http://scalameta.org). The implementations are based on the pre-existing scala.reflect framework and `scala.tools.nsc.Global`.

### How to download

This project is still in a pre-alpha stage, so it's not supposed to be immediately usable just yet.
Nevertheless, we are regularly publishing snapshot builds to Sonatype, so it is already possible to experiment with scala.meta
as described at [https://github.com/scalameta/example](https://github.com/scalameta/example).

As with any snapshot software, things may crash and burn and APIs may noticeably change overnight,
but if you're brave enough, proceed using the instructions, and you may find hidden treasures!
If you have any comments or suggestions during your journey, let us know at
[https://groups.google.com/forum/#!forum/scalameta](https://groups.google.com/forum/#!forum/scalameta).

### How to use

The public API of this project is minimal and consists of a single module: `scala.meta.Scalahost`, which exposes methods to create a `scala.meta.semantic.Context` at compile time (from an instance of `scala.tools.nsc.Global`) and at runtime (from a classpath):

```scala
// compile-time context
import scala.meta._
implicit val c = Scalahost.mkGlobalContext(global)
```

```scala
// runtime context
import scala.meta._
implicit val c = Scalahost.mkStandaloneContext("-cp ...")
```

To see what can be done with a context, consult the readme at [scalameta/scalameta](https://github.com/scalameta/scalameta). To see how this can be done, take a look at an example project at [scalameta/example](https://github.com/scalameta/example).
