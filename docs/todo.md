### Trees

  1. Collection-like methods (see http://clang.llvm.org/docs/LibASTMatchersReference.html)
  1. Rewriting/transformation methods
  1. Unhygienic quasiquotes
  1. Hygiene + hygienic tree equality
  1. What to do with references to particular overloads?
  1. Add moar requires
    1. `require(Interpolate.prefix.isInterpolationId)`
    1. Allowed contexts for Term.Placeholder and Type.Placeholder
    1. Simple type validation
    1. Validate that not all symbols are representable as literals, e.g. scala.Symbol("")
    1. Return type of macro defs must be specified (syntax profile)
    1. Package with hasBraces = false can't be nested in a package with hasBraces = true
    1. Wildcard import can only be the last one in the list of sels
    1. Only non-implicit non-val/var parameters may be by name
    1. `require(within.nonEmpty ==> (within match { case Some(acc: Term.This) => acc.qual.isEmpty; case _ => true }))` for Mod.Private/Protected
  1. Add ast nodes for regular as well as scaladoc comments
  1. Automatically flatten blocks with just a single term?
  1. Revisit Cases.isPartialFunction
  1. Maybe expand @trivia
    1. Distinguishes { x => ... } and (x => { ... })
    1. Maybe merge Def and Procedure using flags?
  1. Trees for macros
  1. Design representation for scaladoc
  1. Possible gaps between docs and related defns are really scary
  1. Revisit Mod.ValParam and Mod.VarParam
  1. Implement validateMods:
    1. https://docs.google.com/spreadsheet/ccc?key=0Ahw_zqMtW4nNdC1lRVJvc3VjTUdOX0ppMVpSYzVRSHc&usp=sharing#gid=0
    1. Write a script that fetches this google doc and converts it into a, say, CSV spec
    1. Write a test that validates the spec by generating source files and parsing them
    1. Write a macro that generates implementation of validateAnnots from the spec + extension methods like isImplicit
  1. `_root_` and `_empty_` are very similar to Qual.Name and might deserve their own qual trees
  1. Trivia: whitespace, comments, etc (see http://msdn.microsoft.com/en-us/vstudio/hh500769)
  1. History vs positions (can trivia be inferred from positions only?)
  1. What should be there in transform? What should be the signature of mapXXX and withXXX?
