### scala.meta
[![Build Status](https://platform-ci.scala-lang.org/api/badges/scalameta/scalameta/status.svg)](https://platform-ci.scala-lang.org/scalameta/scalameta)
[![Join the chat at https://gitter.im/scalameta/scalameta](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/scalameta/scalameta?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

### Stable releases

We try to publish a stable release to Maven Central around the first week of every month.
The latest stable release version number is:
[![Latest stable version](https://index.scala-lang.org/scalameta/scalameta/scalameta/latest.svg?color=orange)](https://index.scala-lang.org/scalameta/scalameta/scalameta)

NOTE. Scala.meta is still under active development so binary compatibility is not
guaranteed between stable releases.

### Non-stable releases

We publish a release to our Bintray repository on every merge into master.
To use these non-stable releases, add this to your build
```scala
resolvers += Resolver.bintrayIvyRepo("scalameta", "maven")
```
The latest non-stable release version number is:
[![Latest snapshot version](https://api.bintray.com/packages/scalameta/maven/scalameta/images/download.svg) ](https://bintray.com/scalameta/maven/scalameta/_latestVersion)

### [User documentation][docs]
Head over to [the user docs][docs] to learn more about the project and its roadmap.

### Tutorial
If you'd like to find out how to use scala.meta, see this [tutorial](http://scalameta.org/tutorial).

### Team
The current maintainers (people who can merge pull requests) are:

* Eugene Burmako - [`@xeno-by`](https://github.com/xeno-by)
* Denys Shabalin - [`@densh`](https://github.com/densh)
* Mikhail Mutcianko - [`@mutcianm`](https://github.com/mutcianm)
* Ólafur Páll Geirsson - [`@olafurpg`](https://github.com/olafurpg)
* David Dudson - [`@DavidDudson`](https://github.com/DavidDudson)

An up-to-date list of contributors is available here: https://github.com/scalameta/scalameta/graphs/contributors.

[docs]: http://scalameta.org
