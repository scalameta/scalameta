package scala.meta.internal
package scalahost
package v1
package online

import java.net.URI
import java.util.UUID.randomUUID
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.compat.Platform.EOL
import scala.util.Properties
import scala.reflect.io.{VirtualFile => GVirtualFile}
import scala.reflect.internal.util.{BatchSourceFile => GBatchSourceFile}
import scala.tools.nsc.reporters.{StoreReporter => GStoreReporter}
import scala.tools.nsc.Global
import scala.meta._
import scala.meta.semantic.v1.Database
import scala.meta.semantic.v1.{Mirror => MirrorApi}
import scala.meta.internal.ast.Helpers._
import scala.meta.internal.scalahost.v1.{Mirror => BaseMirror}

class Mirror(val global: Global)
    extends MirrorApi
    with BaseMirror
    with DatabaseOps
    with ParseOps
    with SymbolOps
    with ReflectionToolkit {

  override def toString: String = {
    val compiler = s"the Scala compiler ${Properties.versionString}"
    val settings = global.settings.toConciseString
    s"online mirror for $compiler running with $settings"
  }

  def sources: Seq[Source] = {
    g.currentRun.units.toList.map(_.toSource)
  }

  def database: Database = {
    var unmappedNames = ""
    val units         = g.currentRun.units.toList ++ adhocUnits.toList
    val databases = units.map(unit => {
      try unit.toDatabase
      catch {
        case ex: Exception if ex.getMessage.startsWith("Unmapped names in") =>
          unmappedNames += (ex.getMessage + EOL)
          Database(Map())
      }
    })
    if (unmappedNames != "") sys.error(unmappedNames.trim)
    val symbols = databases.flatMap(_.symbols).toMap
    Database(symbols)
  }

  private val adhocUnits = mutable.ListBuffer[g.CompilationUnit]()
  def typecheck(tree: Tree): Tree = {
    if (isUnpositioned(tree)) {
      def typecheckRoot(tree: Tree): Tree = {
        tree.parent match {
          case Some(parent) =>
            val index   = parent.children.indexOf(tree)
            val parent1 = typecheckRoot(parent)
            parent1.children(index)
          case _ =>
            def wrapAndTypecheck(uri: URI, member: Stat): Tree = {
              // NOTE: We stringify and reparse to obtain unique positions.
              // That's done because semantic APIs don't work without positions.
              val objectName = Term.fresh("scalahost$")
              val wrapper    = q"object $objectName { $member }".syntax.parse[Source].get
              minputMap(wrapper.pos.input) = uri
              val member1 = wrapper.children(0).children(1).children(1)

              val abstractFile = new GVirtualFile(randomUUID.toString)
              gfileMap(abstractFile) = uri
              val sourceFile  = new GBatchSourceFile(abstractFile, wrapper.syntax)
              val unit        = new g.CompilationUnit(sourceFile)
              val oldReporter = g.reporter
              val reporter    = new GStoreReporter()
              g.reporter = reporter
              try g.currentRun.compileLate(unit)
              finally g.reporter = oldReporter

              if (reporter.hasErrors) {
                val error = reporter.infos.find(_.severity == reporter.ERROR).get
                val pos = {
                  if (tree.pos != Position.None) {
                    val start =
                      Point.Offset(tree.pos.input, error.pos.start - member.pos.start.offset)
                    val end = Point.Offset(tree.pos.input, error.pos.end - member.pos.end.offset)
                    Position.Range(tree.pos.input, start, end)
                  } else {
                    Position.None
                  }
                }
                val msg = error.msg
                throw new SemanticException(pos, msg)
              } else {
                def removeFromUnitbuf(unit: g.CompilationUnit): Unit = {
                  val m_unitbuf = g.currentRun.getClass.getDeclaredMethod("unitbuf")
                  m_unitbuf.setAccessible(true)
                  val unitbuf = m_unitbuf.invoke(g.currentRun)
                  val m_underlying =
                    unitbuf.getClass.getDeclaredMethods.find(_.getName.endsWith("underlying")).get
                  m_underlying.setAccessible(true)
                  val underlying = m_underlying
                    .invoke(unitbuf)
                    .asInstanceOf[mutable.ArrayBuffer[g.CompilationUnit]]
                  underlying -= unit
                }
                g.currentRun.compiledFiles -= unit.source.file.path
                removeFromUnitbuf(unit)
                adhocUnits += unit
                member1
              }
            }
            tree match {
              case tree: Term =>
                val uri     = new URI(s"term:${tree.syntax.trim}")
                val member  = q"val ${Pat.fresh("scalahost$")} = { $tree }"
                val member1 = wrapAndTypecheck(uri, member)
                member1.children(1).children(0)
              case tree: Type =>
                val uri     = new URI(s"type:${tree.syntax.trim}")
                val member  = q"type ${Type.fresh("scalahost$")} = $tree"
                val member1 = wrapAndTypecheck(uri, member)
                member1.children(1)
              case _ =>
                val what        = "quasiquotes that are neither terms nor types"
                val restriction = s"implementation restriction: semantic API doesn't support $what"
                throw new SemanticException(tree.pos, restriction)
            }
        }
      }
      typecheckRoot(tree)
    } else {
      var message =
        s"implementation restriction: semantic API doesn't support mixed unattributed/attributed trees.$EOL"
      message += "For more information, visit https://github.com/scalameta/scalameta/issues/621."
      throw new SemanticException(tree.pos, message)
    }
  }

  private def isUnpositioned(tree: Tree): Boolean = {
    var allUnpositioned = true
    object traverser extends Traverser {
      override def apply(tree: Tree): Unit = {
        if (tree.pos == Position.None) {
          super.apply(tree)
        } else {
          allUnpositioned = false
          return
        }
      }
    }
    traverser(tree.root)
    allUnpositioned
  }
}
