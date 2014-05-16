### Scala host

Scala host for [Project Palladium](http://scalamacros.org/news/2014/03/02/project-palladium.html) provides a
macro plugin that can host Project Palladium macros by overriding macro expansion hooks exposed
by scalac typechecker and wrapping scalac internal compiler data stuctures to conform to the API exposed in
[Palladium core](https://github.com/scalareflect/core).

### How to use

The project is in a very early stage, so it's not supposed to be useful just yet. However, if you're brave enough, we have a nightly build that publishes artifacts to Sonatype at `"org.scalareflect" % "scalahost_2.11.0" % "0.1.0-SNAPSHOT"`.