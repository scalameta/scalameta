### AST interpreter

Typed AST interpreter for [scala.meta](http://scalameta.org). Along with AST persistence, this is a key component of the new macro engine that defeats the separate compilation restrictions and makes macro expansions hostable in non-scalac environments (Intellij, runtime reflection, etc).

### How to use

The project is in a very early stage, so it's not supposed to be useful just yet. However, if you're brave enough, we have a nightly build that publishes artifacts to Sonatype at `"org.scalameta" % "interpreter_2.11" % "0.1.0-SNAPSHOT"`.
