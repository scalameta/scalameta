package scala.meta
package internal
package ast

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.{ListBuffer, ListMap}
import org.scalameta.unreachable
import scala.compat.Platform.EOL
import scala.meta.internal.ast.{Reflection => AstReflection}
import org.scalameta.internal.MacroHelpers

// @ast is a specialized version of @org.scalameta.adt.leaf for scala.meta ASTs.
class ast extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AstNamerMacros.impl
}

class AstNamerMacros(val c: Context) extends AstReflection with CommonNamerMacros {
  lazy val u: c.universe.type = c.universe
  lazy val mirror = c.mirror
  import c.universe._
  import Flag._

  def impl(annottees: Tree*): Tree = annottees.transformAnnottees(new ImplTransformer {
    override def transformClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
      def fullName = c.internal.enclosingOwner.fullName.toString + "." + cdef.name.toString
      def abbrevName = fullName.stripPrefix("scala.meta.")
      def is(abbrev: String) = abbrevName == abbrev
      def isQuasi = cdef.name.toString == "Quasi"
      def isName = is("Name.Anonymous") || is("Name.Indeterminate") || is("Term.Name") || is("Type.Name") || is("Ctor.Ref.Name")
      def isLit = !isQuasi && abbrevName.startsWith("Lit")
      def isCtorRef = !isQuasi && abbrevName.startsWith("Ctor.Ref")
      def isCtorCall = !isQuasi && abbrevName.startsWith("Ctor.Call")
      def isCtorName = !isQuasi && abbrevName.startsWith("Ctor.Ref.Name")
      def looksLikeTermButNotTerm = is("Term.Param") || is("Term.Arg.Named") || is("Term.Arg.Repeated")
      def isTerm = !isQuasi && (abbrevName.startsWith("Term") || isLit || isCtorRef || isCtorCall) && !looksLikeTermButNotTerm
      def isTermName = !isQuasi && is("Term.Name")
      def isTermParam = !isQuasi && is("Term.Param")
      def hasEnv = isName || isTerm
      def hasDenot = isName
      def hasTyping = isTerm || isTermParam
      val q"$imods class $iname[..$tparams] $ctorMods(...$rawparamss) extends { ..$earlydefns } with ..$iparents { $aself => ..$stats }" = cdef
      // TODO: For stack traces, we'd like to have short class names, because stack traces print full names anyway.
      // However debugging macro expansion errors is much-much easier with full names for Api and Impl classes
      // because the typechecker only uses short names in error messages.
      // E.g. compare:
      //  class Impl needs to be abstract, since method withDenot in trait Name
      //  of type (denot: scala.meta.internal.semantic.Denotation)Impl.this.ThisType is not defined
      // and:
      //  class NameAnonymousImpl needs to be abstract, since method withDenot in trait Name
      //  of type (denot: scala.meta.internal.semantic.Denotation)NameAnonymousImpl.this.ThisType is not defined
      val descriptivePrefix = fullName.stripPrefix("scala.meta.").replace(".", "")
      val name = TypeName(descriptivePrefix.replace(".", "") + "Impl")
      // val name = TypeName("Impl")
      val qname = TypeName("Quasi")
      val qmname = TermName("Quasi")
      val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" = mdef
      val bparams1 = ListBuffer[ValDef]() // boilerplate params
      val paramss1 = ListBuffer[List[ValDef]]() // payload params
      val iself = noSelfType
      val self = aself
      val istats1 = ListBuffer[Tree]()
      val stats1 = ListBuffer[Tree]()
      val ianns1 = ListBuffer[Tree]() ++ imods.annotations
      def imods1 = imods.mapAnnotations(_ => ianns1.toList)
      def mods1 = Modifiers(FINAL, mname.toTypeName, List(SerialVersionUIDAnnotation(1L)))
      val iparents1 = ListBuffer[Tree]() ++ iparents
      def parents1 = List(tq"$iname")
      val mstats1 = ListBuffer[Tree]() ++ mstats
      val manns1 = ListBuffer[Tree]() ++ mmods.annotations
      def mmods1 = mmods.mapAnnotations(_ => manns1.toList)

      // step 1: validate the shape of the class
      if (imods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @ast classes")
      if (imods.hasFlag(FINAL)) c.abort(cdef.pos, "final is redundant for @ast classes")
      if (imods.hasFlag(CASE)) c.abort(cdef.pos, "case is redundant for @ast classes")
      if (imods.hasFlag(ABSTRACT)) c.abort(cdef.pos, "@ast classes cannot be abstract")
      if (ctorMods.flags != NoFlags) c.abort(cdef.pos, "@ast classes must define a public primary constructor")
      if (rawparamss.length == 0) c.abort(cdef.pos, "@leaf classes must define a non-empty parameter list")

      // step 2: validate the body of the class
      val (defns, rest1) = stats.partition(_.isDef)
      val (imports, rest2) = rest1.partition(_ match { case _: Import => true; case _ => false })
      stats1 ++= defns
      stats1 ++= imports
      val (requires, illegal) = rest2.partition(_ match { case q"require($what)" => true; case _ => false })
      illegal.foreach(stmt => c.abort(stmt.pos, "only invariants and definitions are allowed in @ast classes"))

      // step 3: calculate the parameters of the class
      val paramss = rawparamss

      // step 4: turn all parameters into vars, create getters and setters
      def internalize(name: TermName) = TermName("_" + name.toString)
      val fieldParamss = paramss
      istats1 ++= fieldParamss.flatten.map(p => {
        var getterAnns = List(q"new $AstMetadataModule.astField")
        if (p.mods.annotations.exists(_.toString.contains("auxiliary"))) getterAnns :+= q"new $AstMetadataModule.auxiliary"
        val getterMods = Modifiers(DEFERRED, typeNames.EMPTY, getterAnns)
        q"$getterMods def ${p.name}: ${p.tpt}"
      })
      paramss1 ++= fieldParamss.map(_.map{
        case p @ q"$mods val $name: $tpt = $default" =>
          val mods1 = mods.mkMutable.unPrivate.unOverride.unDefault
          q"$mods1 val ${internalize(p.name)}: $tpt"
      })
      stats1 ++= fieldParamss.flatten.map(p => {
        val pinternal = internalize(p.name)
        val pmods = if (p.mods.hasFlag(OVERRIDE)) Modifiers(OVERRIDE) else NoMods
        q"""
          $pmods def ${p.name}: ${p.tpt} = {
            $CommonTyperMacrosModule.loadField(this.$pinternal)
            this.$pinternal
          }
        """
      })

      // step 5: implement the unimplemented methods in InternalTree (part 1)
      bparams1 += q"private[meta] val privateFlags: $FlagsClass"
      bparams1 += q"@$TransientAnnotation private[meta] val privatePrototype: $iname"
      bparams1 += q"private[meta] val privateParent: $TreeClass"
      bparams1 += q"private[meta] val privateOrigin: $OriginClass"
      if (hasEnv) bparams1 += q"private[meta] override val privateEnv: $EnvironmentClass"
      if (hasDenot) bparams1 += q"private[meta] override val privateDenot: $DenotationClass"
      if (hasTyping) bparams1 += q"private[meta] override val privateTyping: $TypingClass"

      // step 6: implement the unimplemented methods in InternalTree (part 1)
      // The purpose of privateCopy is to provide extremely cheap cloning
      // in the case when a tree changes its parent (because that happens often in our framework,
      // e.g. when we create a quasiquote and then insert it into a bigger quasiquote,
      // or when we parse something and build the trees from the ground up).
      // In such a situation, we copy all private state verbatim (tokens, denotations, etc)
      // and create lazy initializers that will take care of recursively copying the children.
      // Compare this with the `copy` method (described below), which additionally flushes the private state.
      // This method is private[meta] because the state that it's managing is not supposed to be touched
      // by the users of the framework.
      val privateCopyBargs = ListBuffer[Tree]()
      privateCopyBargs += q"flags"
      privateCopyBargs += q"prototype.asInstanceOf[$iname]"
      privateCopyBargs += q"parent"
      privateCopyBargs += q"origin"
      if (hasEnv) privateCopyBargs += q"env"
      if (hasDenot) privateCopyBargs += q"denot"
      if (hasTyping) privateCopyBargs += q"typing"
      val privateCopyArgs = paramss.map(_.map(p => q"$CommonTyperMacrosModule.initField(this.${internalize(p.name)})"))
      val privateCopyBody = q"new $name(..$privateCopyBargs)(...$privateCopyArgs)"
      stats1 += q"""
        private[meta] def privateCopy(
            flags: $FlagsClass = privateFlags,
            prototype: $TreeClass = this,
            parent: $TreeClass = privateParent,
            origin: $OriginClass = privateOrigin,
            env: $EnvironmentClass = privateEnv,
            denot: $DenotationClass = privateDenot,
            typing: $TypingClass = privateTyping): Tree = {
          $privateCopyBody
        }
      """

      // step 7: create the copy method
      // The purpose of this method is to provide a facility to change small parts of the tree
      // without modifying the other parts, much like the standard case class copy works.
      // In such a situation, the tree is going to be recreated.
      // NOTE: Can't generate XXX.Quasi.copy, because XXX.Quasi already inherits XXX.copy,
      // and there can't be multiple overloaded methods with default parameters.
      // Not a big deal though, since XXX.Quasi is an internal class.
      if (!isQuasi) {
        val fieldDefaultss = fieldParamss.map(_.map(p => q"this.${p.name}"))
        val copyParamss = fieldParamss.zip(fieldDefaultss).map{ case (f, d) => f.zip(d).map { case (p, default) => q"val ${p.name}: ${p.tpt} = $default" } }
        val copyArgss = fieldParamss.map(_.map(p => q"${p.name}"))
        val copyBody = q"$mname.apply(...$copyArgss)"
        istats1 += q"def copy(...$copyParamss): $iname"
        stats1 += q"def copy(...$copyParamss): $iname = $copyBody"
      }

      // step 8: create the children method
      stats1 += q"def children: Seq[$TreeClass] = $CommonTyperMacrosModule.children[$iname, $TreeClass]"

      // step 9: generate boilerplate required by the @ast infrastructure
      // TODO: remove leafClass and leafCompanion from here
      ianns1 += q"new $AstMetadataModule.astClass"
      ianns1 += q"new $AdtMetadataModule.leafClass"
      manns1 += q"new $AstMetadataModule.astCompanion"
      manns1 += q"new $AdtMetadataModule.leafCompanion"

      // step 10: generate boilerplate required by the classifier infrastructure
      mstats1 ++= mkClassifier(iname)

      // step 11: implement Product
      val productParamss = rawparamss.map(_.map(_.duplicate))
      iparents1 += tq"$ProductClass"
      stats1 += q"override def productPrefix: $StringClass = $CommonTyperMacrosModule.productPrefix[$iname]"
      stats1 += q"override def productArity: $IntClass = ${productParamss.head.length}"
      val pelClauses = ListBuffer[Tree]()
      pelClauses ++= 0.to(productParamss.head.length - 1).map(i => cq"$i => this.${productParamss.head(i).name}")
      pelClauses += cq"_ => throw new $IndexOutOfBoundsException(n.toString)"
      stats1 += q"override def productElement(n: $IntClass): Any = n match { case ..$pelClauses }"
      stats1 += q"override def productIterator: $IteratorClass[$AnyClass] = $ScalaRunTimeModule.typedProductIterator(this)"

      // step 12: generate serialization logic
      val fieldInits = fieldParamss.flatten.map(p => q"$CommonTyperMacrosModule.loadField(this.${internalize(p.name)})")
      stats1 += q"protected def writeReplace(): $AnyRefClass = { ..$fieldInits; this }"

      // step 13: generate Companion.apply
      val applyParamss = paramss.map(_.map(_.duplicate))
      val internalParamss = paramss.map(_.map(p => q"@..${p.mods.annotations} val ${p.name}: ${p.tpt}"))
      val internalBody = ListBuffer[Tree]()
      val internalLocalss = paramss.map(_.map(p => (p.name, internalize(p.name))))
      internalBody += q"$CommonTyperMacrosModule.hierarchyCheck[$iname]"
      // internalBody += q"$AdtTyperMacros.immutabilityCheck[$iname]"
      internalBody ++= internalLocalss.flatten.map{ case (local, internal) => q"$DataTyperMacrosModule.nullCheck($local)" }
      internalBody ++= internalLocalss.flatten.map{ case (local, internal) => q"$DataTyperMacrosModule.emptyCheck($local)" }
      internalBody ++= imports
      internalBody ++= requires.map(require => {
        var hasErrors = false
        object errorChecker extends Traverser {
          override def traverse(tree: Tree): Unit = tree match {
            case _: This => hasErrors = true; c.error(tree.pos, "cannot refer to this in @ast requires")
            case _ => super.traverse(tree)
          }
        }
        errorChecker.traverse(require)
        if (hasErrors) q"()"
        else require
      })
      var internalInitCount = 3 // privatePrototype, privateParent, privateOrigin
      if (hasEnv) internalInitCount += 1
      if (hasDenot) internalInitCount += 1
      if (hasTyping) internalInitCount += 1
      val internalInitss = 1.to(internalInitCount).map(_ => q"null")
      val paramInitss = internalLocalss.map(_.map{ case (local, internal) => q"$CommonTyperMacrosModule.initParam($local)" })
      internalBody += q"val node = new $name($FlagsZeroField, ..$internalInitss)(...$paramInitss)"
      internalBody ++= internalLocalss.flatten.flatMap{ case (local, internal) =>
        val (validators, assignee) = {
          // TODO: this is totally ugly. we need to come up with a way to express this in a sane way.
          val validateLocal = TermName("validate" + local.toString.capitalize)
          if (is("Pkg") && local.toString == "stats") {
            val validators = List(q"def $validateLocal(stat: Stat) = { require(stat.isTopLevelStat); stat }")
            (validators, q"$local.map($validateLocal)")
          } else if ((is("Defn.Trait") || is("Defn.Object") || is("Pkg.Object")) && local.toString == "templ") {
            val validators = List(q"def $validateLocal(stats: Seq[Stat]) = stats.map(stat => { require(!stat.is[Ctor]); stat })")
            (validators, q"{ if (!$local.is[$QuasiClass]) $local.stats.map($validateLocal); $local }")
          } else if (is("Template") && local.toString == "early") {
            val validators = List(q"def $validateLocal(stat: Stat) = { require(stat.isEarlyStat && parents.nonEmpty); stat }")
            (validators, q"$local.map($validateLocal)")
          } else if (is("Template") && local.toString == "stats") {
            val validators = List(q"def $validateLocal(stats: Seq[Stat]) = stats.map(stat => { require(stat.isTemplateStat); stat })")
            (validators, q"$local.map($validateLocal)")
          } else {
            (Nil, q"$local")
          }
        }
        validators :+ q"$CommonTyperMacrosModule.storeField(node.$internal, $assignee)"
      }
      internalBody += q"node"
      val internalArgss = paramss.map(_.map(p => q"${p.name}"))
      mstats1 += q"""
        def apply(...$applyParamss): $iname = {
          def internal(...$internalParamss): $iname = {
            ..$internalBody
          }
          internal(...$internalArgss)
        }
      """

      // step 14: generate Companion.unapply
      val unapplyParamss = rawparamss.map(_.map(_.duplicate))
      val unapplyParams = unapplyParamss.head
      val needsUnapply = !mstats.exists(stat => stat match { case DefDef(_, TermName("unapply"), _, _, _, _) => true; case _ => false })
      if (needsUnapply) {
        if (unapplyParams.length != 0) {
          // TODO: re-enable name-based pattern matching once https://issues.scala-lang.org/browse/SI-9029 is fixed
          // stats1 += q"@$InlineAnnotation final def isDefined = !isEmpty"
          // stats1 += q"@$InlineAnnotation final def isEmpty = false"
          // val getBody = if (unapplyParams.length == 1) q"this.${unapplyParams.head.name}" else q"this"
          // stats1 += q"@$InlineAnnotation final def get = $getBody"
          // unapplyParams.zipWithIndex.foreach({ case (p, i) => stats1 += q"@$InlineAnnotation final def ${TermName("_" + (i + 1))} = this.${p.name}" })
          // mstats1 += q"@$InlineAnnotation final def unapply(x: $name): $name = x"
          val successTargs = tq"(..${unapplyParams.map(p => p.tpt)})"
          val successArgs = q"(..${unapplyParams.map(p => q"x.${p.name}")})"
          mstats1 += q"@$InlineAnnotation final def unapply(x: $iname): $OptionClass[$successTargs] = if (x == null) $NoneModule else $SomeModule($successArgs)"
        } else {
          mstats1 += q"@$InlineAnnotation final def unapply(x: $iname): $BooleanClass = true"
        }
      }

      // step 15: finish codegen for Quasi
      if (isQuasi) {
        stats1 += q"""
          def become[T <: $QuasiClass](implicit ev: $AstInfoClass[T]): T = {
            this match {
              case $mname(0, tree) =>
                ev.quasi(0, tree).withOrigin(this.origin).asInstanceOf[T]
              case $mname(1, nested @ $mname(0, tree)) =>
                ev.quasi(1, nested.become[T]).withOrigin(this.origin).asInstanceOf[T]
              case _ =>
                throw new Exception("complex ellipses are not supported yet")
            }
          }
        """
      } else {
        // NOTE: additional stubs account for Pkg.name and Name.Anonymous.value
        mstats1 += mkQuasi(iname, iparents, fieldParamss, "name", "value")
      }

      mstats1 += q"$mods1 class $name[..$tparams] $ctorMods(...${bparams1 +: paramss1}) extends { ..$earlydefns } with ..$parents1 { $self => ..$stats1 }"
      val cdef1 = q"$imods1 trait $iname extends ..$iparents1 { $iself => ..$istats1 }"
      val mdef1 = q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
      if (c.compilerSettings.contains("-Xprint:typer")) { println(cdef1); println(mdef1) }
      List(cdef1, mdef1)
    }
  })
}