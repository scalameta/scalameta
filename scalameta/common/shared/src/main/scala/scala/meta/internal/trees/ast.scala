package scala.meta
package internal
package trees

import scala.language.experimental.macros
import scala.annotation.{StaticAnnotation, tailrec}
import scala.collection.mutable.ListBuffer
import scala.math.Ordered.orderingToOrdered
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
        val mstatsLatest = ListBuffer[Tree]()
        val manns1 = ListBuffer[Tree]() ++ mmods.annotations
        def mmods1 = mmods.mapAnnotations(_ => manns1.toList)
        val quasiCopyExtraParamss = ListBuffer[List[ValDef]]()
        val quasiExtraAbstractDefs = ListBuffer[ValOrDefDef]()

        // step 1: validate the shape of the class
        if (imods.hasFlag(SEALED)) c.abort(cdef.pos, "sealed is redundant for @ast classes")
        if (imods.hasFlag(FINAL)) c.abort(cdef.pos, "final is redundant for @ast classes")
        if (imods.hasFlag(CASE)) c.abort(cdef.pos, "case is redundant for @ast classes")
        if (imods.hasFlag(ABSTRACT)) c.abort(cdef.pos, "@ast classes cannot be abstract")
        if (ctorMods.flags != NoFlags)
          c.abort(cdef.pos, "@ast classes must define a public primary constructor")
        if (rawparamss.isEmpty)
          c.abort(cdef.pos, "@leaf classes must define a non-empty parameter list")
        if (rawparamss.lengthCompare(1) > 0)
          c.abort(cdef.pos, "@leaf classes must define a single parameter list")
        val params = rawparamss.head

        // step 1a: identify modified fields of the class
        val versionedParams = if (isQuasi) Nil else getVersionedParams(params, stats)
        val paramsVersions = versionedParams.flatMap(_.getVersions).distinct.sorted
        val replacedFields = versionedParams.flatMap(_.replaced.flatMap { field =>
          field.oldDefs.map { case (oldDef, _) => field.version -> oldDef }
        })
        def paramsForVersion(v: Version): List[ValDef] =
          positionVersionedParams(versionedParams.flatMap(_.getApplyDeclDefnBefore(v)._1))

        // step 2: validate the body of the class

        var needCopies = !isQuasi
        val importsBuilder = List.newBuilder[Import]
        val checkFieldsBuilder = List.newBuilder[Tree]
        val checkParentsBuilder = List.newBuilder[Tree]

        stats.foreach {
          case x: Import => importsBuilder += x
          case x: DefDef if !isQuasi && x.name == TermName("copy") =>
            istats1 += x; needCopies = false
          case x: ValOrDefDef =>
            if (x.mods.hasFlag(Flag.ABSTRACT) || x.rhs.isEmpty)
              c.abort(x.pos, "definition without a value")
            val p = replacedFields
              .collectFirst { case (version, `x`) =>
                val mods = x.mods.mapAnnotations(getDeprecatedAnno(version) :: _)
                q"$mods def ${x.name}: ${x.tpt} = ${x.rhs}"
              }
              .getOrElse(x)
            if (x.mods.hasFlag(Flag.FINAL)) istats1 += p
            else quasiExtraAbstractDefs += p
          case q"checkFields($arg)" => checkFieldsBuilder += arg
          case x @ q"checkParent($what)" => checkParentsBuilder += x
          case x =>
            val error =
              "only checkFields(...), checkParent(...) and definitions are allowed in @ast classes"
            c.abort(x.pos, error)
        }
        val imports = importsBuilder.result()
        val fieldChecks = checkFieldsBuilder.result()
        val parentChecks = checkParentsBuilder.result()

        istats1 ++= imports
        mstats1 ++= imports
        stats1 ++= imports
        stats1 ++= quasiExtraAbstractDefs

        // step 4: turn all parameters into vars, create getters and setters
        params.foreach { p =>
          val getterAnns = q"new $AstMetadataModule.astField" :: p.mods.annotations
          istats1 += declareGetter(p.name, p.tpt, getterAnns)
          val pmods = if (p.mods.hasFlag(OVERRIDE)) Modifiers(OVERRIDE) else NoMods
          stats1 += defineGetter(p.name, p.tpt, pmods)
        }
        paramss1 += params.map { p =>
          val mods1 = p.mods.mkMutable.unPrivate.unOverride.unDefault
          q"$mods1 val ${internalize(p.name)}: ${p.tpt}"
        }

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
        val privateCopyArgs =
          params.map(p => q"$CommonTyperMacrosModule.initField(this.${internalize(p.name)})")
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
            new $name(prototype.asInstanceOf[$iname], parent, origin)(..$privateCopyArgs)
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
        def getParamArg(p: ValDef): TermName = p.name
        def addCopy(params: List[ValDef], annots: Tree*) = {
          val mods = Modifiers(DEFERRED, typeNames.EMPTY, annots.toList)
          istats1 += q"""
            $mods def copy(..$params): $iname
          """
          val args = params.map(getParamArg)
          stats1 += q"""
            final override def copy(..$params): $iname = {
              val newAst = $mname.apply(..$args)
              newAst
            }
          """
          quasiCopyExtraParamss += params
        }
        if (!isQuasi) {
          def getCopyParamWithDefault(p: ValOrDefDef): ValDef = asValDefn(p, q"this.${p.name}")
          val fullCopyParams = params.map(getCopyParamWithDefault)
          val iFullCopy = q"private[meta] def fullCopy(..$fullCopyParams): $iname"
          istats1 += iFullCopy
          quasiExtraAbstractDefs += iFullCopy
          stats1 += q"""
            private[meta] final override def fullCopy(..$fullCopyParams): $iname = {
              val newAst = $mname.apply(..${params.map(_.name)})
              newAst
            }
          """
          if (needCopies) {
            if (versionedParams.isEmpty) {
              addCopy(fullCopyParams)
            } else {
              // add primary copy with default values
              val defaultCopyParams =
                positionVersionedParams(versionedParams.flatMap(_.getDefaultCopyDef()))
                  .map(getCopyParamWithDefault)
              addCopy(defaultCopyParams)

              val defaultCopyParamNames = defaultCopyParams.map(_.name.toString).toSet
              def allInDefaults(cp: Iterable[ValDef]): Boolean =
                cp.forall(x => defaultCopyParamNames.contains(x.name.toString))

              // add full copy without defaults
              val fullCopyParamsNoDefaults = params.map(asValDecl)
              if (!allInDefaults(fullCopyParamsNoDefaults))
                addCopy(fullCopyParamsNoDefaults)
              // add secondary copy
              paramsVersions.foreach { version =>
                val copyParams = paramsForVersion(version)
                if (copyParams.length != defaultCopyParams.length || !allInDefaults(copyParams))
                  addCopy(copyParams, getDeprecatedAnno(version))
              }
            }
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
        iparents1 += tq"$ProductClass"

        stats1 += q"override def productPrefix: $StringClass = $CommonTyperMacrosModule.productPrefix[$iname]"
        stats1 += q"override def productArity: $IntClass = ${params.length}"

        def patternMatchClauses(fromField: (ValDef, Int) => Tree) = {
          val pelClauses = ListBuffer[Tree]()
          pelClauses ++= params.zipWithIndex.map(fromField.tupled)
          pelClauses += cq"_ => throw new $IndexOutOfBoundsException(n.toString)"
          pelClauses.toList
        }

        val pelClauses = patternMatchClauses((vr, i) => cq"$i => this.${vr.name}")
        stats1 += q"override def productElement(n: $IntClass): Any = n match { case ..$pelClauses }"
        stats1 += q"override def productIterator: $IteratorClass[$AnyClass] = $ScalaRunTimeModule.typedProductIterator(this)"
        val productFields = params.map(_.name.toString)
        stats1 += q"override def productFields: $ListClass[$StringClass] = _root_.scala.List(..$productFields)"

        // step 13a add productElementName for 2.13
        if (MacroCompat.productFieldNamesAvailable) {
          val penClauses = patternMatchClauses { (vr, i) =>
            val lit = Literal(Constant(vr.name.toString()))
            cq"""$i => $lit """
          }
          stats1 += q"override def productElementName(n: $IntClass): java.lang.String = n match { case ..$penClauses }"
        }

        // step 12: generate serialization logic
        stats1 += q"""
          protected def writeReplace(): $AnyRefClass = {
            ..${params.map(loadField)}
            this
          }
        """

        // step 13: generate Companion.apply
        val internalBody = ListBuffer[Tree]()
        internalBody += q"$CommonTyperMacrosModule.hierarchyCheck[$iname]"
        params.foreach { p =>
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
        val paramInits = params.map { p =>
          q"$CommonTyperMacrosModule.initParam(${p.name})"
        }
        internalBody += q"val node = new $name(..$internalInitss)(..$paramInits)"
        params.foreach(p => internalBody += storeField(p))
        internalBody += q"node"
        val applyParams =
          params.map(p => q"@..${p.mods.annotations} val ${p.name}: ${p.tpt} = ${p.rhs}")
        val internalArgs = params.map(getParamArg)
        mstats1 += q"""
          def apply(..$applyParams): $iname = {
            ..$internalBody
          }
        """
        mstatsLatest += q"""
          @$InlineAnnotation def apply(..$applyParams): $iname = $mname.apply(..$internalArgs)
        """

        // step 13a: generate additional companion apply for added and replaced fields
        // generate new applies for each new field added
        // with field A, B and additional binary compat ones C, D and E, we generate:
        // apply(A, B, C), apply(A, B, C, D), apply(A, B, C, D, E)
        paramsVersions.foreach { v =>
          val applyParamsBuilder = List.newBuilder[(ValDef, Int)]
          val applyBodyBuilder = List.newBuilder[Tree]
          versionedParams.foreach { vp =>
            val (decl, defn) = vp.getApplyDeclDefnBefore(v)
            decl.foreach(applyParamsBuilder += _)
            defn.foreach(applyBodyBuilder += _)
          }
          val params = positionVersionedParams(applyParamsBuilder.result())
          applyBodyBuilder += q"$mname.apply(..$internalArgs)"
          val applyBody = applyBodyBuilder.result()
          mstats1 += q"@${getDeprecatedAnno(v)} def apply(..$params): $iname = { ..$applyBody }"
        }

        // step 14: generate Companion.unapply
        val needsUnapply = !mstats.exists {
          case DefDef(_, TermName("unapply"), _, _, _, _) => true
          case _ => false
        }
        if (needsUnapply) {
          def getUnapply(unapplyParams: List[ValDef], annots: Tree*): Tree = {
            val successTargs = tq"(..${unapplyParams.map(p => p.tpt)})"
            val successArgs = q"(..${unapplyParams.map(p => q"x.${p.name}")})"
            q"""
              @$InlineAnnotation @..$annots final def unapply(x: $iname): $OptionClass[$successTargs] =
                if (x != null && x.isInstanceOf[$name]) $SomeModule($successArgs) else $NoneModule
            """
          }
          if (params.nonEmpty) {
            val latestTree = getUnapply(params)
            mstats1 += paramsVersions.headOption.fold {
              latestTree
            } { ver =>
              getUnapply(paramsForVersion(ver), getDeprecatedAnno(ver))
            }
            mstatsLatest += latestTree
          } else {
            val tree = q"@$InlineAnnotation final def unapply(x: $iname): $BooleanClass = true"
            mstats1 += tree
            mstatsLatest += tree
          }
        }

        // step 15: finish codegen for Quasi
        if (isQuasi) {
          stats1 += q"""
          def become[T <: $TreeClass](implicit ev: $AstInfoClass[T]): T with $QuasiClass = {
            (this match {
              case $mname(0, tree) =>
                ev.quasi(0, tree)
              case $mname(rank, nested @ $mname(0, tree)) =>
                ev.quasi(rank, nested.become[T])
              case _ =>
                throw new Exception("complex ellipses are not supported yet")
            }).withOrigin(this.origin): T with $QuasiClass
          }
        """
        } else {
          mstats1 += mkQuasi(
            iname,
            iparents,
            params,
            quasiCopyExtraParamss,
            quasiExtraAbstractDefs.result(),
            "name",
            "value",
            "tpe"
          )
        }

        mstats1 += q"""
          object internal { // to be ignored by Mima
            object Latest {
              ..$mstatsLatest
            }
          }
        """

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

  private class VersionedParam(
      val param: ValDef,
      val appended: Option[Version],
      val replaced: Seq[ReplacedField]
  ) {
    appended.foreach { aver =>
      replaced.headOption.foreach { rfield =>
        if (rfield.version <= aver) {
          val oldDef = rfield.oldDefs.head._1
          c.abort(
            param.pos,
            s"$aver [@newField for ${param.name}] must must precede " +
              s"${rfield.version} [@replacedField for ${oldDef.name}]"
          )
        }
      }
    }

    def getVersions: Iterable[Version] = appended ++ replaced.map(_.version)
    def getApplyDeclDefnBefore(version: Version): (List[(ValDef, Int)], Option[ValDef]) = {
      def checkVersion(ver: Version): Boolean = version <= ver
      if (appended.exists(checkVersion)) (Nil, Some(asValDefn(param)))
      else
        replaced
          .find(x => checkVersion(x.version))
          .map { rfield =>
            val decls = rfield.oldDefs.map { case (oldDef, pos) => asValDecl(oldDef) -> pos }
            (decls, Some(rfield.newValDefn))
          }
          .getOrElse((asValDecl(param) -> -1 :: Nil, None))
    }
    def getDefaultCopyDef(): List[(ValOrDefDef, Int)] = {
      replaced.headOption.map(_.oldDefs).getOrElse((param, -1) :: Nil)
    }
  }

  private def positionVersionedParams[A](params: List[(A, Int)]): List[A] = {
    val res = new ListBuffer[A]
    val paramIter = params.iterator.filter(_._2 < 0)
    @tailrec
    def iter(withPositions: List[(A, Int)]): Unit = {
      withPositions match {
        case (v, pos) :: rest =>
          paramIter.take(pos - res.length).foreach { case (x, _) => res += x }
          res += v
          iter(rest)
        case _ =>
          paramIter.foreach { case (x, _) => res += x }
      }
    }
    iter(params.filter(_._2 >= 0).sortBy(_._2))
    res.toList
  }

  private def getVersionedParams(
      params: List[ValDef],
      stats: List[Tree]
  ): List[VersionedParam] = {
    val appendedFields: Map[String, Version] = getNewFieldVersions(params)
    val replacedFields: Map[String, Seq[ReplacedField]] = ReplacedField.getMap(params, stats)
    params.map { p =>
      val pname = p.name.toString
      new VersionedParam(p, appendedFields.get(pname), replacedFields.getOrElse(pname, Seq.empty))
    }
  }

  private def getAnnotAttribute(value: Tree): String =
    value match {
      case x: AssignOrNamedArg => x.rhs.toString
      case x => x.toString
    }

  private def parseVersionAnnot(version: Tree, annot: String, field: String): Version = {
    val parsed = Version
      .parse(getAnnotAttribute(version).stripPrefix("\"").stripSuffix("\""))
      .getOrElse(c.abort(version.pos, s"@$annot must contain $field=major.minor.patch"))
    majorVersion.foreach { major =>
      if (parsed.major < major)
        c.abort(version.pos, s"@$annot: obsolete, old major version (must be $major)")
    }
    parsed
  }

  private def getNewFieldVersions(params: List[ValDef]): Map[String, Version] = {
    val builder = Map.newBuilder[String, Version]
    var prevVersion: Version = null
    params.foreach { x =>
      val sinceOpt = x.mods.annotations.collectFirst { case q"new newField($since)" => since }
      if (sinceOpt.isEmpty && prevVersion != null)
        c.abort(x.pos, "must be marked @newField since previous field is")
      sinceOpt.foreach { since =>
        if (x.mods.hasFlag(Flag.OVERRIDE))
          c.abort(x.pos, "override fields may not be marked @newField")
        if (x.rhs == EmptyTree)
          c.abort(x.pos, "@newField fields must provide a default value")
        val version = parseVersionAnnot(since, "newField", "since")
        if (null != prevVersion && version < prevVersion)
          c.abort(x.pos, s"previous field marked with newer version: $prevVersion")
        prevVersion = version
        builder += x.name.toString -> version
      }
    }
    builder.result()
  }

  private class ReplacedField(
      val version: Version,
      val newVal: ValDef,
      ctor: Tree,
      val oldDefs: List[(ValOrDefDef, Int)]
  ) {
    def newValDefn: ValDef = {
      def bodyForSingleOldDef(oldDef: ValOrDefDef) =
        if (ctor eq null)
          q"""
            import scala.meta.trees._
            ${oldDef.name}
           """
        else
          q"""
            $ctor(${oldDef.name})
           """
      def bodyForMultipleOldDefs = {
        if (ctor eq null) c.abort(newVal.pos, s"${newVal.name} must define a ctor")
        val names = oldDefs.map { case (oldDef, _) =>
          val name = q"${oldDef.name}"
          val arg = AssignOrNamedArg(name, name)
          q"$arg"
        }
        q"$ctor(..$names)"
      }
      val body = oldDefs match {
        case (oldDef, _) :: Nil => bodyForSingleOldDef(oldDef)
        case _ => bodyForMultipleOldDefs
      }
      q"""
        val ${newVal.name}: ${deannotateType(newVal)} = {
          ..$body
        }
      """
    }
  }

  private object ReplacedField {
    def getMap(
        params: List[ValDef],
        stats: List[Tree]
    ): Map[String, Seq[ReplacedField]] = {
      val fields: Map[String, (ValDef, Map[Version, Tree])] = params.map { p =>
        val ctorsByVersion = p.mods.annotations.collect {
          case q"new replacesFields($since, $ctor)" =>
            val version = parseVersionAnnot(since, "replacesFields", "since")
            version -> ctor
        }.toMap
        p.name.toString -> (p, ctorsByVersion)
      }.toMap
      val replacedFields = stats.flatMap {
        case p: ValOrDefDef =>
          val anno = p.mods.annotations.collectFirst {
            case q"new replacedField($until)" => (until, -1)
            case q"new replacedField($until, $pos)" =>
              (until, getAnnotAttribute(pos).toInt)
          }
          anno.map { case (until, pos) =>
            if (!p.mods.hasFlag(Flag.FINAL))
              c.abort(p.pos, "replacedField-annotated fields must be final")
            val version = parseVersionAnnot(until, "replacedField", "until")
            val newField = getNewField(p)
            newField -> (p, version, pos)
          }
        case _ => None
      }
      replacedFields.groupBy(_._1).map { case (k, v) =>
        val (newVal, ctorsByVersion) = fields.getOrElse(
          k,
          c.abort(v.head._2._1.pos, s"@replacedField: field `$k` is undefined)")
        )
        val replacements = v.map(_._2).groupBy(_._2).toSeq.map { case (ver, oldFields) =>
          val ctor = ctorsByVersion.get(ver).orNull
          val oldDefs = oldFields.map { case (oldField, _, pos) => (oldField, pos) }
          new ReplacedField(ver, newVal, ctor, oldDefs)
        }
        k -> replacements.sortBy(_.version)
      }
    }

    private def getNewField(oldDef: ValOrDefDef): String = {
      @tailrec
      def iter(tree: Tree): Option[String] = tree match {
        case Select(Ident(TermName(newField)), _: TermName) => Some(newField)
        case Apply(_, Ident(TermName(newField)) :: Nil) => Some(newField)
        case Match(Ident(TermName(newField)), _) => Some(newField)
        case Select(x, _) => iter(x)
        case Apply(x, (_: Function) :: Nil) => iter(x)
        case _ => None
      }
      iter(oldDef.rhs).getOrElse(
        c.abort(oldDef.pos, s"@replacedField: can't find new field name (${showRaw(oldDef.rhs)})")
      )
    }
  }

  private def getDeprecatedAnno(v: Version) =
    q"new scala.deprecated(${Literal(Constant(v.toString))})"

  private def asValDecl(p: ValOrDefDef): ValDef =
    q"val ${p.name}: ${deannotateType(p)}"
  private def asValDefn(p: ValOrDefDef): ValDef = asValDefn(p, p.rhs)
  private def asValDefn(p: ValOrDefDef, rhs: Tree): ValDef =
    q"val ${p.name}: ${deannotateType(p)} = $rhs"
  private def deannotateType(p: ValOrDefDef): Tree = p.tpt match {
    case Annotated(_, tpt) => tpt
    case tpt => tpt
  }

}

object AstNamerMacros {

  private val majorVersion = {
    val buildVersion: String = BuildInfo.version
    val majorIdx = buildVersion.indexOf('.')
    if (majorIdx > 0) Some(buildVersion.substring(0, majorIdx).toInt) else None
  }

}
