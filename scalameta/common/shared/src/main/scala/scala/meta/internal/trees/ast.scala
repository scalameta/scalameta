package scala.meta
package internal
package trees

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.reflect.macros.whitebox.Context
import scala.collection.mutable.ListBuffer
import org.scalameta.internal.MacroCompat

// @ast is a specialized version of @org.scalameta.adt.leaf for scala.meta ASTs.
class ast extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AstNamerMacros.impl
}

class AstNamerMacros(val c: Context) extends Reflection with CommonNamerMacros {
  lazy val u: c.universe.type = c.universe
  lazy val mirror = c.mirror
  import c.universe._
  import Flag._

  def impl(annottees: Tree*): Tree =
    annottees.transformAnnottees(new ImplTransformer {
      override def transformClass(cdef: ClassDef, mdef: ModuleDef): List[ImplDef] = {
        val fullName = c.internal.enclosingOwner.fullName + "." + cdef.name.toString
        def isQuasi = isQuasiClass(cdef)
        val q"$imods class $iname[..$tparams] $ctorMods(...$rawparamss) extends { ..$earlydefns } with ..$iparents { $aself => ..$stats }" =
          cdef
        // NOTE: For stack traces, we'd like to have short class names, because stack traces print full names anyway.
        // However debugging macro expansion errors is much-much easier with full names for Api and Impl classes
        // because the typechecker only uses short names in error messages.
        // E.g. compare:
        //  class Impl needs to be abstract, since method withDenot in trait Name
        //  of type (denot: scala.meta.internal.semantic.Denotation)Impl.this.ThisType is not defined
        // and:
        //  class NameAnonymousImpl needs to be abstract, since method withDenot in trait Name
        //  of type (denot: scala.meta.internal.semantic.Denotation)NameAnonymousImpl.this.ThisType is not defined
        val descriptivePrefix = fullName.stripPrefix("scala.meta.").replace(".", "")
        val name = TypeName(descriptivePrefix + "Impl")
        val q"$mmods object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats }" =
          mdef
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
        if (ctorMods.flags != NoFlags)
          c.abort(cdef.pos, "@ast classes must define a public primary constructor")
        if (rawparamss.isEmpty)
          c.abort(cdef.pos, "@leaf classes must define a non-empty parameter list")

        // step 2: validate the body of the class

        def setterName(vr: ValDef) =
          TermName(s"set${vr.name.toString.stripPrefix("_").capitalize}")
        def getterName(vr: ValDef) = TermName(s"${vr.name.toString.stripPrefix("_")}")

        def binaryCompatMods(vr: ValDef) = {
          val getter = q"def ${getterName(vr)}: ${vr.tpt}"
          getter.mods.mapAnnotations(_ => vr.mods.annotations)
        }

        def isBinaryCompatField(vr: ValDef) = vr.mods.annotations.exists {
          case q"new binaryCompatField($since)" =>
            if (!vr.name.toString.startsWith("_"))
              c.abort(vr.pos, "The binaryCompat AST field needs to start with _")
            if (!vr.mods.hasFlag(PRIVATE))
              c.abort(vr.pos, "The binaryCompat AST field needs to be private")
            if (!vr.mods.hasFlag(MUTABLE))
              c.abort(vr.pos, "The binaryCompat AST field needs to declared as var")
            true
          case _ => false
        }

        val copiesBuilder = List.newBuilder[DefDef]
        val importsBuilder = List.newBuilder[Import]
        val binaryCompatVarsBuilder = List.newBuilder[ValDef]
        val otherDefsBuilder = List.newBuilder[Tree]
        val checkFieldsBuilder = List.newBuilder[Tree]
        val checkParentsBuilder = List.newBuilder[Tree]

        stats.foreach {
          case x: Import => importsBuilder += x
          case x: DefDef if !isQuasi && x.name == TermName("copy") => copiesBuilder += x
          case x: ValDef if isBinaryCompatField(x) => binaryCompatVarsBuilder += x
          case x if x.isDef => otherDefsBuilder += x
          case q"checkFields($arg)" => checkFieldsBuilder += arg
          case x @ q"checkParent($what)" => checkParentsBuilder += x
          case x =>
            val error =
              "only checkFields(...), checkParent(...) and definitions are allowed in @ast classes"
            c.abort(x.pos, error)
        }
        val copies = copiesBuilder.result()
        val imports = importsBuilder.result()
        val binaryCompatVars = binaryCompatVarsBuilder.result()
        val otherDefns = otherDefsBuilder.result()
        val fieldChecks = checkFieldsBuilder.result()
        val parentChecks = checkParentsBuilder.result()

        stats1 ++= otherDefns

        val binaryCompatAbstractFields = binaryCompatVars.flatMap { vr: ValDef =>
          List(
            q"${binaryCompatMods(vr)} def ${getterName(vr)}: ${vr.tpt}",
            q"def ${setterName(vr)}(${vr.name} : ${vr.tpt}) : Unit"
          )
        }
        istats1 ++= binaryCompatAbstractFields

        val binaryCompatStats = binaryCompatVars.flatMap { vr: ValDef =>
          val name = vr.name
          val nameString = name.toString
          List(
            q"def ${getterName(vr)} = this.$name ",
            q"""def ${setterName(vr)}($name : ${vr.tpt}) = {
                   val node = this
                   $CommonTyperMacrosModule.storeField(this.$name, $name, $nameString)
                }""",
            vr
          )
        }
        stats1 ++= binaryCompatStats

        stats1 ++= imports

        // step 3: calculate the parameters of the class
        val paramss = rawparamss

        // step 4: turn all parameters into vars, create getters and setters
        def internalize(name: TermName) = TermName("_" + name.toString)
        val fieldParamss = paramss
        val fieldParams = fieldParamss.flatten.map(p => (p, p.name.decodedName.toString))
        istats1 ++= fieldParams.map({ case (p, _) =>
          var getterAnns = List(q"new $AstMetadataModule.astField")
          if (p.mods.annotations.exists(_.toString.contains("auxiliary")))
            getterAnns :+= q"new $AstMetadataModule.auxiliary"
          val getterMods = Modifiers(DEFERRED, typeNames.EMPTY, getterAnns)
          q"$getterMods def ${p.name}: ${p.tpt}"
        })
        paramss1 ++= fieldParamss.map(_.map { case p @ q"$mods val $name: $tpt = $default" =>
          val mods1 = mods.mkMutable.unPrivate.unOverride.unDefault
          q"$mods1 val ${internalize(p.name)}: $tpt"
        })
        stats1 ++= fieldParams.map({ case (p, s) =>
          val pinternal = internalize(p.name)
          val pmods = if (p.mods.hasFlag(OVERRIDE)) Modifiers(OVERRIDE) else NoMods
          q"""
          $pmods def ${p.name}: ${p.tpt} = {
            $CommonTyperMacrosModule.loadField(this.$pinternal, $s)
            this.$pinternal
          }
        """
        })

        // step 5: implement the unimplemented methods in InternalTree (part 1)
        bparams1 += q"@$TransientAnnotation private[meta] val privatePrototype: $iname"
        bparams1 += q"private[meta] val privateParent: $TreeClass"
        bparams1 += q"private[meta] val privateOrigin: $OriginClass"

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
        privateCopyBargs += q"prototype.asInstanceOf[$iname]"
        privateCopyBargs += q"parent"
        privateCopyBargs += q"origin"
        val binaryCompatCopies = binaryCompatVars.collect { case vr: ValDef =>
          List(q"newAst.${setterName(vr)}(this.${vr.name});")
        }
        val privateCopyArgs = paramss.map(
          _.map(p => q"$CommonTyperMacrosModule.initField(this.${internalize(p.name)})")
        )
        val privateCopyParentChecks = {
          if (parentChecks.isEmpty) q""
          else {
            q"""
            if (destination != null) {
              def checkParent(fn: ($name, $TreeClass, $StringClass) => $BooleanClass): $UnitClass = {
                val parentCheckOk = fn(this, parent, destination)
                if (!parentCheckOk) {
                  val parentPrefix = parent.productPrefix
                  _root_.org.scalameta.invariants.require(parentCheckOk && _root_.org.scalameta.debug(this, parentPrefix, destination))
                }
              }
              ..$parentChecks
            }
          """
          }
        }
        val privateCopyBody =
          q"val newAst = new $name(..$privateCopyBargs)(...$privateCopyArgs); ..$binaryCompatCopies; newAst"
        stats1 += q"""
        private[meta] def privateCopy(
            prototype: $TreeClass = this,
            parent: $TreeClass = privateParent,
            destination: $StringClass = null,
            origin: $OriginClass = privateOrigin): Tree = {
          $privateCopyParentChecks
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
          if (copies.isEmpty) {
            val fieldDefaultss = fieldParamss.map(_.map(p => q"this.${p.name}"))
            val copyParamss = fieldParamss.zip(fieldDefaultss).map { case (f, d) =>
              f.zip(d).map { case (p, default) => q"val ${p.name}: ${p.tpt} = $default" }
            }
            val copyArgss = fieldParamss.map(_.map(p => q"${p.name}"))
            val copyBody = q"$mname.apply(...$copyArgss)"
            istats1 += q"def copy(...$copyParamss): $iname"
            stats1 += q"""def copy(...$copyParamss): $iname = {val newAst = $copyBody; ..$binaryCompatCopies; newAst}"""
          } else {
            istats1 ++= copies
          }
        }

        // step 7a: override the Object and Equals methods
        if (!isQuasi) {
          istats1 += q"final override def canEqual(that: Any): $BooleanClass = that.isInstanceOf[$iname]"
          istats1 += q"final override def equals(that: Any): $BooleanClass = this eq that.asInstanceOf[AnyRef]"
          istats1 += q"final override def hashCode: $IntClass = System.identityHashCode(this)"
          istats1 += q"final override def toString: $StringClass = scala.meta.internal.prettyprinters.TreeToString(this)"
        }

        // step 8: create the children method
        stats1 += q"def children: $ListClass[$TreeClass] = $CommonTyperMacrosModule.children[$iname, $TreeClass]"

        // step 9: generate boilerplate required by the @ast infrastructure
        ianns1 += q"new $AstMetadataModule.astClass"
        ianns1 += q"new $AdtMetadataModule.leafClass"
        manns1 += q"new $AstMetadataModule.astCompanion"
        manns1 += q"new $AdtMetadataModule.leafCompanion"

        // step 10: generate boilerplate required by the classifier infrastructure
        mstats1 ++= mkClassifier(iname)

        // step 11: implement Product
        val binaryCompatNum = binaryCompatVars.size
        val productParamss = rawparamss.map(_.map(_.duplicate))
        iparents1 += tq"$ProductClass"

        stats1 += q"override def productPrefix: $StringClass = $CommonTyperMacrosModule.productPrefix[$iname]"
        stats1 += q"override def productArity: $IntClass = ${productParamss.head.length + binaryCompatNum}"

        def patternMatchClauses(
            fromField: Int => Tree,
            fromBinaryCompatField: (Int, ValDef) => Tree
        ) = {
          val pelClauses = ListBuffer[Tree]()
          val fieldsNum = productParamss.head.length
          pelClauses ++= 0
            .to(fieldsNum - 1)
            .map(fromField)

          // generate product elements for @binaryCompat fields
          pelClauses ++= fieldsNum
            .to(fieldsNum + binaryCompatNum)
            .zip(binaryCompatVars)
            .map { case (i: Int, vr: ValDef) =>
              fromBinaryCompatField(i, vr)
            }
          pelClauses += cq"_ => throw new $IndexOutOfBoundsException(n.toString)"
          pelClauses.toList
        }

        val pelClauses = patternMatchClauses(
          i => cq"$i => this.${productParamss.head(i).name}",
          { (i, vr) =>
            cq"$i => this.${getterName(vr)}"
          }
        )
        stats1 += q"override def productElement(n: $IntClass): Any = n match { case ..$pelClauses }"
        stats1 += q"override def productIterator: $IteratorClass[$AnyClass] = $ScalaRunTimeModule.typedProductIterator(this)"
        val productFields = productParamss.head.map(_.name.toString) ++ binaryCompatVars.map {
          vr: ValDef => getterName(vr).toString
        }
        stats1 += q"override def productFields: $ListClass[$StringClass] = _root_.scala.List(..$productFields)"

        // step 13a add productElementName for 2.13
        if (MacroCompat.productFieldNamesAvailable) {
          val penClauses = patternMatchClauses(
            i => {
              val lit = Literal(Constant(productParamss.head(i).name.toString(): String))
              cq"""$i => $lit """
            },
            { (i, vr) =>
              val lit = Literal(Constant(getterName(vr).toString: String))
              cq"""$i => $lit """
            }
          )
          stats1 += q"override def productElementName(n: $IntClass): java.lang.String = n match { case ..$penClauses }"
        }
        // step 12: generate serialization logic
        val fieldInits = fieldParams.map({ case (p, s) =>
          q"$CommonTyperMacrosModule.loadField(this.${internalize(p.name)}, $s)"
        })
        stats1 += q"protected def writeReplace(): $AnyRefClass = { ..$fieldInits; this }"

        // step 13: generate Companion.apply
        val applyParamss = paramss.map(_.map(_.duplicate))
        val internalParamss =
          paramss.map(_.map(p => q"@..${p.mods.annotations} val ${p.name}: ${p.tpt}"))
        val internalBody = ListBuffer[Tree]()
        val internalLocalss = paramss.map(_.map(p => (p.name, internalize(p.name))))
        internalBody += q"$CommonTyperMacrosModule.hierarchyCheck[$iname]"
        internalBody ++= internalLocalss.flatten.map { case (local, internal) =>
          q"$DataTyperMacrosModule.nullCheck($local)"
        }
        internalBody ++= internalLocalss.flatten.map { case (local, internal) =>
          q"$DataTyperMacrosModule.emptyCheck($local)"
        }
        internalBody ++= imports
        internalBody ++= fieldChecks.map { x =>
          val fieldCheck = q"_root_.org.scalameta.invariants.require($x)"
          var hasErrors = false
          object errorChecker extends Traverser {
            private val nmeParent = TermName("parent")
            override def traverse(tree: Tree): Unit = tree match {
              case _: This =>
                hasErrors = true; c.error(tree.pos, "cannot refer to this in @ast field checks")
              case Ident(`nmeParent`) =>
                hasErrors = true
                c.error(
                  tree.pos,
                  "cannot refer to parent in @ast field checks; use checkParent instead"
                )
              case _ => super.traverse(tree)
            }
          }
          errorChecker.traverse(fieldCheck)
          if (hasErrors) q"()"
          else fieldCheck
        }
        val internalInitCount = 3 // privatePrototype, privateParent, privateOrigin
        val internalInitss = 1.to(internalInitCount).map(_ => q"null")
        val paramInitss = internalLocalss.map(_.map { case (local, internal) =>
          q"$CommonTyperMacrosModule.initParam($local)"
        })
        internalBody += q"val node = new $name(..$internalInitss)(...$paramInitss)"
        internalBody ++= internalLocalss.flatten.map { case (local, internal) =>
          q"$CommonTyperMacrosModule.storeField(node.$internal, $local, ${local.toString})"
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

        // step 13a: generate additional binary compat Companion.apply
        // generate new applies for each new field added
        // with field A, B and additional binary compat ones C, D and E, we generate:
        // apply(A, B, C), apply(A, B, C, D), apply(A, B, C, D, E)
        val sortedBinaryCompatName = binaryCompatVars
          .collect { case vr: ValDef => vr }
          .sortBy { vr =>
            try {
              vr.mods.annotations.collectFirst { case q"new binaryCompatField($version)" =>
                val since = version.toString().stripPrefix("\"").stripSuffix("\"")
                val versions = since.split("\\.").map(_.toInt)
                (versions(0), versions(1), versions(2))
              }.get
            } catch {
              case _: Throwable =>
                c.abort(
                  vr.pos,
                  "binaryCompatField annotation must contain since=\"major.minor.patch\" field"
                )
            }
          }

        1.to(binaryCompatNum).foreach { size =>
          val fields = sortedBinaryCompatName.take(size)
          val paramFields = fields.map(f => q"val ${getterName(f)} : ${f.tpt}")
          val params = List(applyParamss.head ++ paramFields) ++ applyParamss.tail
          val setters = fields.map { vr =>
            q"newAst.${setterName(vr)}(${getterName(vr)});"
          }

          val checks = fields.flatMap { field =>
            List(
              q"$DataTyperMacrosModule.nullCheck(${getterName(field)})",
              q"$DataTyperMacrosModule.emptyCheck(${getterName(field)})"
            )
          }
          mstats1 += q"""
              def apply(...$params): $iname = {
                ..$checks
                val newAst = apply(...$internalArgss);
                ..$setters
                newAst
              }
              """

        }

        // step 14: generate Companion.unapply
        val needsUnapply = !mstats.exists {
          case DefDef(_, TermName("unapply"), _, _, _, _) => true
          case _ => false
        }
        if (needsUnapply) {
          if (rawparamss.head.nonEmpty) {
            val unapplyParams = rawparamss.head.map(_.duplicate)
            val successTargs = tq"(..${unapplyParams.map(p => p.tpt)})"
            val successArgs = q"(..${unapplyParams.map(p => q"x.${p.name}")})"
            mstats1 += q"""
              @$InlineAnnotation final def unapply(x: $iname): $OptionClass[$successTargs] =
                if (x != null && x.isInstanceOf[$name]) $SomeModule($successArgs) else $NoneModule
            """
          } else {
            mstats1 += q"@$InlineAnnotation final def unapply(x: $iname): $BooleanClass = true"
          }
        }

        // step 15: finish codegen for Quasi
        if (isQuasi) {
          stats1 += q"""
          def become[T <: $TreeClass](implicit ev: $AstInfoClass[T]): T with $QuasiClass = {
            (this match {
              case $mname(0, tree) =>
                ev.quasi(0, tree)
              case $mname(1, nested @ $mname(0, tree)) =>
                ev.quasi(1, nested.become[T])
              case $mname(2, nested @ $mname(0, tree)) =>
                ev.quasi(2, nested.become[T])
              case _ =>
                throw new Exception("complex ellipses are not supported yet")
            }).withOrigin(this.origin): T with $QuasiClass
          }
        """
        } else {
          mstats1 += mkQuasi(
            iname,
            iparents,
            fieldParamss,
            binaryCompatAbstractFields ++ otherDefns,
            "name",
            "value",
            "tpe"
          )
        }

        mstats1 += q"$mods1 class $name[..$tparams] $ctorMods(...${bparams1 +: paramss1}) extends { ..$earlydefns } with ..$parents1 { $self => ..$stats1 }"
        val cdef1 = q"$imods1 trait $iname extends ..$iparents1 { $iself => ..$istats1 }"
        val mdef1 =
          q"$mmods1 object $mname extends { ..$mearlydefns } with ..$mparents { $mself => ..$mstats1 }"
        if (c.compilerSettings.contains("-Xprint:typer")) {
          println(cdef1); println(mdef1)
        }
        List(cdef1, mdef1)
      }
    })
}
