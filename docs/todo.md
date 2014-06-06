### Trees

  1. Collection-like methods (see http://clang.llvm.org/docs/LibASTMatchersReference.html)
  1. Rewriting/transformation methods
  1. Hygiene and hygienic equalities (trees, bound_==, ref_==)
    1. What to do with references to particular overloads?
  1. Add moar requires
    1. `require(Interpolate.prefix.isInterpolationId)`
    1. Allowed contexts for Term.Placeholder and Type.Placeholder
    1. Simple type validation (see syntax summary for simple vs non-simple types)
    1. Validate that not all symbol literals are representable as literals, e.g. scala.Symbol("")
    1. Return type of macro defs must be specified (wrt syntax profile)
    1. Package with hasBraces = false can't be nested in a package with hasBraces = true
    1. Wildcard import can only be the last one in the list of sels; also can only occur once in the sels list
    1. Only non-implicit non-val/var parameters may be by name
    1. `require(within.nonEmpty ==> (within match { case Some(acc: Term.This) => acc.qual.isEmpty; case _ => true }))` for Mod.Private/Protected
  1. Add ast nodes for regular as well as scaladoc comments
  1. Automatically flatten blocks with just a single term?
  1. Revisit Cases vs PartialFunction representation
  1. Maybe expand @trivia
    1. Maybe merge Def and Procedure using defaults?
  1. Trees for macros (esp old vs new macro)
  1. Design representation for scaladoc and comments in general
  1. Possible gaps between docs and related defns are really scary, consider making docs a semantic operation on definition
  1. Revisit class val/var parameter representation (Mod.ValParam vs separate tree)
  1. Implement mods validation:
    1. https://docs.google.com/spreadsheet/ccc?key=0Ahw_zqMtW4nNdC1lRVJvc3VjTUdOX0ppMVpSYzVRSHc&usp=sharing#gid=0
    1. Write a script that fetches this google doc and converts it into a, say, CSV spec
    1. Write a test that validates the spec by generating source files and parsing them
    1. Write a macro that generates implementation of validateAnnots from the spec + extension methods like isImplicit
  1. `_root_` and `_empty_` are very similar to Qual.Super and might deserve their own qual trees
  1. Trivia: whitespace, comments, etc (see http://msdn.microsoft.com/en-us/vstudio/hh500769)
  1. History vs positions (can trivia be inferred from positions only?)
  1. What should be there in Origin.Transform? What should be the signature of mapXXX and withXXX (should they take Origin or not)?
  1. Liftable[Tree]

### Quasiquotes

  1. Port current unhygienic quasiquotes
  1. Consider native placeholder trees
  1. Better ellipses (e.g. q"..{ $a = $b }" to match/construct list of assigns)
