While converting symbols and types from a platform-dependent representation to scala.meta poses a certain challenge,
the main difficulty in writing the converter is converting trees, and that'll be the focus of this document for now.

Below you can find a comprehensive list of desugarings that we have to deal with in scalahost.
Supporting more and more desugarings brings us closer and closer to fixing
[#148](https://github.com/scalameta/scalameta/issues/148), [#235](https://github.com/scalameta/scalameta/issues/235)
and releasing v0.0.5 of scala.meta.

To understand the theoretical background and the notation that we employ in the tables below,
scroll down to the "Theory" & "Practice" sections in the end of the document.

### Term desugarings

<table>
  <th>
    <td width="50px">Meta</td>
    <td width="50px">Scalac</td>
    <td width="50px">Dotc</td>
    <td>Description</td>
  </th>
  <tr>
    <td valign="top">E1</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Non-local names are desugared into fully-qualified references.</td>
  </tr>
  <tr>
    <td valign="top">E2</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">References to names imported via renaming imports are desugared into fully-qualified references to the original definition with the renamed name destroyed.</td>
  </tr>
  <tr>
    <td valign="top">E3</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Blocks that contain a single statement are collapsed.</td>
  </tr>
  <tr>
    <td valign="top">E4</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">If the expected type in Unit, then blocks, which don't end with an expression typed as Unit, get a () appended to their body.</td>
  </tr>
  <tr>
    <td valign="top">E5</td>
    <td valign="top">N</td>
    <td valign="top">+<sup>1</sup></td>
    <td valign="top">+<sup>2</sup></td>
    <td valign="top">Constant folding: 1) replacing expressions typed with constant types with the underlying literal of the constant types, 2) ...</td>
  </tr>
  <tr>
    <td valign="top">E6</td>
    <td valign="top">Y</td>
    <td valign="top">+<sup>3</sup></td>
    <td valign="top">-<sup>4</sup></td>
    <td valign="top">Macro expansion.</td>
  </tr>
  <tr>
    <td valign="top">E7</td>
    <td valign="top">Y</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Nullary calls to empty-paramlist methods get an empty arglist.</td>
  </tr>
  <tr>
    <td valign="top">E8</td>
    <td valign="top">Y</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Unary calls get transformed into nullary calls to unary_XXX.</td>
  </tr>
  <tr>
    <td valign="top">E9</td>
    <td valign="top">Y</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Inference of type arguments.</td>
  </tr>
  <tr>
    <td valign="top">E10</td>
    <td valign="top">Y</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Eta expansion.</td>
  </tr>
  <tr>
    <td valign="top">E11</td>
    <td valign="top">Y</td>
    <td valign="top">+</td>
    <td valign="top">+</td>
    <td valign="top">`apply` insertion.</td>
  </tr>
  <tr>
    <td valign="top">E12</td>
    <td valign="top">Y</td>
    <td valign="top">+</td>
    <td valign="top">+</td>
    <td valign="top">`update` insertion.</td>
  </tr>
  <tr>
    <td valign="top">E13</td>
    <td valign="top">Y</td>
    <td valign="top">+</td>
    <td valign="top">+</td>
    <td valign="top">Implicit arguments.</td>
  </tr>
  <tr>
    <td valign="top">E14</td>
    <td valign="top">Y</td>
    <td valign="top">+</td>
    <td valign="top">+</td>
    <td valign="top">`for` loops.</td>
  </tr>
</table>

<sup>1</sup> Original tracked by scalahost, reached a preliminary agreement with Jason to submit a pull request to scala/scala.<br/>
<sup>2</sup> Dotty doesn't constfold in typer, but the subsequent phase messes things up. A conversation with Dmitry and Martin has concluded with an admission that this is a bug.<br/>
<sup>3</sup> Already supported by scalac since 2.10.x via MacroExpansionAttachment.<br/>
<sup>4</sup> Dotty doesn't have macros. Yet.<br/>

### Member desugarings

<table>
  <th>
    <td width="50px">Meta</td>
    <td width="50px">Scalac</td>
    <td width="50px">Dotc</td>
    <td>Description</td>
  </th>
  <tr>
    <td valign="top">M1</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Vals, vars, parameters and methods with unspecified type get the type inferred.</td>
  </tr>
  <tr>
    <td valign="top">M2</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Nullary constructors get desugared to empty-paramlist constructors.</td>
  </tr>
  <tr>
    <td valign="top">M3</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Empty parent lists get appended with AnyRef.</td>
  </tr>
  <tr>
    <td valign="top">M4</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Case class parent lists get appended with Product and Serializable.</td>
  </tr>
  <tr>
    <td valign="top">M5</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Repeated occurrences of Product, ProductN and Serializable in parent lists are weeded out.</td>
  </tr>
  <tr>
    <td valign="top">M6</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Parent lists that start with Any get Any converted to AnyRef.</td>
  </tr>
  <tr>
    <td valign="top">M7</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Parent lists that start with a trait different from Any get prepended with tpe.firstParent.</td>
  </tr>
  <tr>
    <td valign="top">M8</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Nullary parents get converted to empty-arglist parents.</td>
  </tr>
  <tr>
    <td valign="top">M9</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Empty lower and upper bounds get converted to Nothing and Any respectively.</td>
  </tr>
</table>

### Extra desugarings

<table>
  <th>
    <td width="50px">Meta</td>
    <td width="50px">Scalac</td>
    <td width="50px">Dotc</td>
    <td>Description</td>
  </th>
  <tr>
    <td valign="top">X1</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Variable patterns are represented as binds to wildcard patterns.</td>
  </tr>
  <tr>
    <td valign="top">X2</td>
    <td valign="top">N</td>
    <td valign="top">+</td>
    <td valign="top">?</td>
    <td valign="top">Typed patterns are represented as binds to wildcard typed patterns.</td>
  </tr>
</table>
### Notation

The `Meta` column indicates whether the desugaring manifests itself in scala.meta via Term.desugar (Y) or not (N). The `Scalac` column indicates whether scalac performs the desugaring (+) or not (-). The `Dotc` column has the same information for Dotty, with an additional possible value of ? (because I don't know Dotty well enough).

### Theory

One of the foundational principles of scala.meta is that we don't lose information about source code.
A corollary of this principle is that **in scala.meta we can't desugar trees even if the underlying host does**.
This means that the seemingly mechanical task of converting platform-specific trees into platform-independent trees
becomes really non-trivial.

While thinking how to reconcile the objective reality of desugarings and the idealistic dream of principled metaprogramming,
we arrived at the following conclusions:
  * There are three kinds of desugarings that Scala compilers perform:
    **1) rewritings that lower very high-level language features,
    2) approximations of Scala syntax with scala.reflect trees,
    3) convenience desugarings that aren't necessary at all but are done to simplify the compiler internally.**
  * In scalac, Type-1 desugarings are typically done in typer, type-2 desugarings mostly happen in parser,
    and type-3 desugarings are sprinkled all around the code.
  * In dotc, both type-1 and type-2 desugarings are performed in typer
    (because dotty's parser trees are much closer to Scala syntax than scalac's).
    Type-3 desugarings are yet to be found, which is very much appreciated.
  * While type-2 and type-3 desugarings are incidental complexity, only necessary because of implementation details,
    type-1 desugarings are actually useful - for certain code analyses like find usages
    and for certain operations like AST interpretation.
  * TASTY persistence, as well as scala.meta conversion, happens after typer in both scalac and dotc,
    which means that we can't sidestep the necessity of dealing with desugarings.

### Practice

While the general problem of detecting and undoing desugarings in a typechecked tree is impossible to solve,
there is an infrastructural trick that gives hope. The thing is that in most situations,
scala.meta can obtain original sources of desugared trees (either by downloading xxx-sources.jar for a Maven-based artifact
or by requesting the user for a source directory of a project being analyzed).

**We hypothesize that having parser ASTs alongside post-typer ASTs should be enough to handle most type-1 and type-2 desugarings**.
Concretely, we believe that for the overwhelming majority of desugarings, it is possible to take an unattributed original tree
together with its attributed desugaring and then to fill in attributes of the original based on the desugaring.

Some type-1 desugarings (e.g. macro expansion) and all type-3 desugarings (e.g. collapse of `List[T]()` into `Nil` in scalac)
still present a challenge, because it's either impossible to recognize the desugaring by structural comparison (in the case of
macro expansion) or impossible to attribute certain parts of the original from the desugaring alone (the `T` part the case
of the Nil collapse). **At the moment, we don't have a good idea how to handle these special desugarings, so we have to special-case them
in scalac and dotc compilers.**

Following this theoretical basis, in scalahost we have three modules that convert platform-specific trees into platform-independent trees:
  1. [ToMtree.scala](/scalahost/src/main/scala/scala/meta/internal/hosts/scalac/converters/ToMtree.scala) that converts either unattributed or attributed scalac trees into, correspondingly, unattributed or attributed scala.meta trees as is, without undoing any desugarings. This is a relatively trivial part of the converter that consists in recognizing platform-dependent encodings of different Scala syntax and mechanically transforming them into scala.meta encodings.
  2. [MergeTrees.scala](/scalameta/trees/src/main/scala/scala/meta/internal/ast/MergeTrees.scala) that merges unattributed original trees and attributed desugared trees, undoing most type-1 and type-2 desugarings. This is most challenging part of the converter, because we have to detect and handle dozens of different desugarings, hoping for the best.
  3. [UninferrableDesugarings.scala](/scalahost/src/main/scala/scala/meta/internal/hosts/scalac/reflect/UninferrableDesugarings.scala) that keeps track of originals for the rest of type-1 and type-3 desugarings. Not much logic here - we expect someone to solve our problem for us by remembering originals, and we just enjoy the results of someone else's work by reading those originals from predefined locations.
