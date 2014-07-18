### Scala host

Scala host for [scala.meta](http://scalameta.org) provides a
macro plugin that can host Project Palladium macros by overriding macro expansion hooks exposed
by scalac typechecker and wrapping scalac internal compiler data stuctures to conform to the API exposed in
[the core](https://github.com/scalameta/scalameta).

### How to use

The project is in a very early stage, so it's not supposed to be useful just yet. However, if you're brave enough, we have a nightly build that publishes artifacts to Sonatype at `"org.scalameta" % "scalahost_2.11.1" % "0.1.0-SNAPSHOT"`.
