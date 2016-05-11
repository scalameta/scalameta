### scala.meta

[![Join the chat at https://gitter.im/scalameta/scalameta](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/scalameta/scalameta?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

Scala.meta is based on a principle that metaprogramming should be completely decoupled from compiler internals.
This project is a clean-room implementation of a metaprogramming toolkit for Scala, designed to be simple, robust and portable.
We are striving for scala.meta to become a successor of scala.reflect, the current de facto standard in Scala ecosystem.

### Why scala.meta

Based on our experiences with developing scala.tools.nsc and scala.reflect, we have designed scala.meta to have a comprehensive data model that supports all syntactic and semantic details of Scala. As a result, scala.meta has a unique architecture that allows it to implement functionality that is very hard or outright impossible to achieve with existing metaprogramming solutions.

### How to use

Even though scala.meta hasn't yet reached 1.0, there is a number of early adopters who have successfully used our project to solve real-life metaprogramming problems. We're happy to see that scala.meta is genuinely useful and are currently preparing detailed documentation to share its power with everyone.

### How to contribute

We have been lucky to receive a number of important contributions from a lot of enthusiasts -
both internally at EPFL and externally over the internet. If you are interested in leaving your mark
in the design and implementation of the metaprogramming toolkit of the future, check out
[the list of available issues](https://github.com/scalameta/scalameta/issues?q=is%3Aopen+is%3Aissue+label%3A%22Contributor+alert%22) and start hacking!
