package scala.meta
package internal
package trees

import scala.language.experimental.macros
import scala.annotation.StaticAnnotation
import scala.collection.mutable.ListBuffer
import scala.reflect.macros.whitebox.Context

import org.scalameta.internal.MacroCompat

// @ast is a specialized version of @org.scalameta.adt.leaf for scala.meta ASTs.
class ast extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro AstNamerMacros.impl
}

class AstNamerMacros(val c: Context) extends Reflection with CommonNamerMacros {
  import AstNamerMacros._

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

        def isBinaryCompatField(vr: ValDef) = vr.mods.annotations.exists {
          case q"new binaryCompatField($since)" =>
            val name = vr.name.toString
            if (name != internalize(name).toString)
              c.abort(vr.pos, "The binaryCompat AST field needs to start with _")
            if (!vr.mods.hasFlag(PRIVATE))
              c.abort(vr.pos, "The binaryCompat AST field needs to be private")
            if (!vr.mods.hasFlag(MUTABLE))
              c.abort(vr.pos, "The binaryCompat AST field needs to declared as var")
            true
          case _ => false
        }

        var needCopies = !isQuasi
        val importsBuilder = List.newBuilder[Import]
        val binaryCompatVarsBuilder = List.newBuilder[ValDef]
        val otherDefsBuilder = List.newBuilder[Tree]
        val checkFieldsBuilder = List.newBuilder[Tree]
        val checkParentsBuilder = List.newBuilder[Tree]

        stats.foreach {
          case x: Import => importsBuilder += x
          case x: DefDef if !isQuasi && x.name == TermName("copy") =>
            istats1 += x; needCopies = false
          case x: ValDef if isBinaryCompatField(x) => binaryCompatVarsBuilder += x
          case x if x.isDef => otherDefsBuilder += x
          case q"checkFields($arg)" => checkFieldsBuilder += arg
          case x @ q"checkParent($what)" => checkParentsBuilder += x
          case x =>
            val error =
              "only checkFields(...), checkParent(...) and definitions are allowed in @ast classes"
            c.abort(x.pos, error)
        }
        val imports = importsBuilder.result()
        val binaryCompatVars = binaryCompatVarsBuilder.result()
        val otherDefns = otherDefsBuilder.result()
        val fieldChecks = checkFieldsBuilder.result()
        val parentChecks = checkParentsBuilder.result()

        val binaryCompatAbstractFields = binaryCompatVars.flatMap { vr =>
          val name = vr.name
          val tpt = vr.tpt
          stats1 += vr
          stats1 += q"override def ${getterName(name)}: $tpt = this.$name"
          stats1 += defineSetter(name, tpt, Modifiers(OVERRIDE))
          List(declareGetter(name, tpt, vr.mods.annotations), declareSetter(name, tpt, Nil))
        }
        istats1 ++= binaryCompatAbstractFields

        stats1 ++= otherDefns
        stats1 ++= imports

        // step 3: calculate the parameters of the class
        val paramss = rawparamss

        // step 4: turn all parameters into vars, create getters and setters
        val fieldParamss = paramss
        val fieldParams = fieldParamss.flatten
        fieldParams.foreach { p =>
          val getterAnns = q"new $AstMetadataModule.astField" :: p.mods.annotations
          istats1 += declareGetter(p.name, p.tpt, getterAnns)
          val pmods = if (p.mods.hasFlag(OVERRIDE)) Modifiers(OVERRIDE) else NoMods
          stats1 += defineGetter(p.name, p.tpt, pmods)
        }
        fieldParamss.foreach(paramss1 += _.map { p =>
          val mods1 = p.mods.mkMutable.unPrivate.unOverride.unDefault
          q"$mods1 val ${internalize(p.name)}: ${p.tpt}"
        })

        // step 5: implement the unimplemented methods in InternalTree (part 1)
        val bparams1 = List(
          q"@$TransientAnnotation private[meta] val privatePrototype: $iname",
          q"private[meta] val privateParent: $TreeClass",
          q"private[meta] val privateOrigin: $OriginClass"
        )

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
        stats1 += q"""
        private[meta] def privateCopy(
            prototype: $TreeClass = this,
            parent: $TreeClass = privateParent,
            destination: $StringClass = null,
            origin: $OriginClass = privateOrigin): Tree = {
          $privateCopyParentChecks
          val newAst =
            new $name(prototype.asInstanceOf[$iname], parent, origin)(...$privateCopyArgs)
          ..$binaryCompatCopies
          newAst
        }
      """
        // step 7: create the copy method
        // The purpose of this method is to provide a facility to change small parts of the tree
        // without modifying the other parts, much like the standard case class copy works.
        // In such a situation, the tree is going to be recreated.
        // NOTE: Can't generate XXX.Quasi.copy, because XXX.Quasi already inherits XXX.copy,
        // and there can't be multiple overloaded methods with default parameters.
        // Not a big deal though, since XXX.Quasi is an internal class.
        def addCopy(paramss: List[List[ValDef]], argss: List[List[Tree]], annots: Tree*) = {
          val mods = Modifiers(DEFERRED, typeNames.EMPTY, annots.toList)
          istats1 += q"""
            $mods def copy(...$paramss): $iname
          """
          stats1 += q"""
            final override def copy(...$paramss): $iname = {
              val newAst = $mname.apply(...$argss)
              ..$binaryCompatCopies
              newAst
            }
          """
        }
        if (needCopies) {
          val fieldDefaultss = fieldParamss.map(_.map(p => q"this.${p.name}"))
          val copyParamss = fieldParamss.zip(fieldDefaultss).map { case (f, d) =>
            f.zip(d).map { case (p, default) => q"val ${p.name}: ${p.tpt} = $default" }
          }
          val copyArgss = fieldParamss.map(_.map(p => q"${p.name}"))
          addCopy(copyParamss, copyArgss)
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
        stats1 += q"""
          protected def writeReplace(): $AnyRefClass = {
            ..${fieldParams.map(loadField)}
            this
          }
        """

        // step 13: generate Companion.apply
        val internalBody = ListBuffer[Tree]()
        internalBody += q"$CommonTyperMacrosModule.hierarchyCheck[$iname]"
        fieldParams.foreach { p =>
          val local = p.name
          internalBody += q"$DataTyperMacrosModule.nullCheck(${local})"
          internalBody += q"$DataTyperMacrosModule.emptyCheck(${local})"
        }
        internalBody ++= imports
        fieldChecks.foreach { x =>
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
          if (!hasErrors) internalBody += fieldCheck
        }
        val internalInitCount = 3 // privatePrototype, privateParent, privateOrigin
        val internalInitss = 1.to(internalInitCount).map(_ => q"null")
        val paramInitss = paramss.map(_.map { p =>
          q"$CommonTyperMacrosModule.initParam(${p.name})"
        })
        internalBody += q"val node = new $name(..$internalInitss)(...$paramInitss)"
        fieldParams.foreach(p => internalBody += storeField(p))
        internalBody += q"node"
        val applyParamss =
          paramss.map(_.map(p => q"@..${p.mods.annotations} val ${p.name}: ${p.tpt}"))
        val internalArgss = paramss.map(_.map(p => q"${p.name}"))
        mstats1 += q"""
          def apply(...$applyParamss): $iname = {
            ..$internalBody
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
                parseVersion(version.toString())
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
          def getUnapply(unapplyParams: List[ValDef]): Tree = {
            val successTargs = tq"(..${unapplyParams.map(p => p.tpt)})"
            val successArgs = q"(..${unapplyParams.map(p => q"x.${p.name}")})"
            q"""
              @$InlineAnnotation final def unapply(x: $iname): $OptionClass[$successTargs] =
                if (x != null && x.isInstanceOf[$name]) $SomeModule($successArgs) else $NoneModule
            """
          }
          val unapplyParamsAll = rawparamss.head
          if (unapplyParamsAll.nonEmpty) {
            mstats1 += getUnapply(unapplyParamsAll)
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
            Nil,
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

  private def internalize(name: String): TermName = TermName(s"_${name.stripPrefix("_")}")
  private def internalize(name: TermName): TermName = internalize(name.toString)
  private def setterName(name: String): TermName =
    TermName(s"set${name.stripPrefix("_").capitalize}")
  private def setterName(name: TermName): TermName = setterName(name.toString)
  private def setterName(vr: ValOrDefDef): TermName = setterName(vr.name)
  private def getterName(name: String): TermName = TermName(s"${name.stripPrefix("_")}")
  private def getterName(name: TermName): TermName = getterName(name.toString)
  private def getterName(vr: ValOrDefDef): TermName = getterName(vr.name)

  private def loadField(vr: ValOrDefDef): Tree = loadField(vr.name)
  private def loadField(name: TermName): Tree = loadField(internalize(name), name)
  private def loadField(internalName: TermName, name: TermName): Tree =
    q"""
      $CommonTyperMacrosModule.loadField(this.$internalName, ${name.decodedName.toString})
    """

  private def storeField(vr: ValOrDefDef): Tree = storeField(vr.name)
  private def storeField(name: TermName): Tree = storeField(internalize(name), name)
  private def storeField(internalName: TermName, name: TermName): Tree =
    q"""
      $CommonTyperMacrosModule.storeField(node.$internalName, $name, ${name.decodedName.toString})
    """

  private def declareGetter(name: TermName, tpe: Tree, annots: List[Tree]): Tree =
    declareGetter(name, tpe, Modifiers(DEFERRED, typeNames.EMPTY, annots))

  private def declareGetter(name: TermName, tpe: Tree, mods: Modifiers): Tree =
    q"$mods def ${getterName(name)}: $tpe"

  private def defineGetter(name: TermName, tpe: Tree, mods: Modifiers): Tree = {
    val internalName = internalize(name)
    q"""
      $mods def ${getterName(name)}: $tpe = {
        ${loadField(internalName, name)}
        this.$internalName
      }
    """
  }

  private def declareSetter(name: TermName, tpe: Tree, annots: List[Tree]): Tree =
    declareSetter(name, tpe, Modifiers(DEFERRED, typeNames.EMPTY, annots))

  private def declareSetter(name: TermName, tpe: Tree, mods: Modifiers): Tree =
    q"$mods def ${setterName(name)}($name : $tpe): Unit"

  private def defineSetter(name: TermName, tpe: Tree, mods: Modifiers): Tree = {
    q"""
      $mods def ${setterName(name)}($name : $tpe): Unit = {
        val node = this
        ${storeField(name)}
      }
    """
  }

}

object AstNamerMacros {

  private type Version = (Int, Int, Int)

  private def parseVersion(v: String): Version = {
    val since = v.stripPrefix("\"").stripSuffix("\"")
    val versions = since.split('.').map(_.toInt)
    (versions(0), versions(1), versions(2))
  }

}
