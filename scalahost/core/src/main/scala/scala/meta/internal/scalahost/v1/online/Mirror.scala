package scala.meta.internal
package scalahost
package v1
package online

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
import scala.meta.semantic.v1.{Mirror => MirrorApi}
import scala.meta.internal.ast.Helpers._
import scala.meta.internal.scalahost.v1.{Mirror => BaseMirror}

class Mirror(val global: Global)
    extends MirrorApi
    with BaseMirror
    with DatabaseOps
    with DenotationOps
    with ParseOps
    with ReporterOps
    with SymbolOps
    with ReflectionToolkit {

  override def toString: String = {
    val compiler = s"the Scala compiler ${Properties.versionString}"
    val settings = global.settings.toConciseString
    s"online mirror for $compiler running with $settings"
  }

  private val adhocUnits = mutable.ListBuffer[g.CompilationUnit]()
  private def compilerUnits: Seq[g.CompilationUnit] = {
    g.currentRun.units.filter(!_.source.file.name.endsWith(".java")).toList
  }

  def sources: Seq[Source] = {
    compilerUnits.toList.map(_.toSource)
  }

  private var cachedDatabaseKey = (g.currentRun, -1, -1)
  private def recomputeCachedDatabaseKey() = (g.currentRun, g.currentRun.size, adhocUnits.size)
  private var cachedDatabase: Database = null
  def database: Database = {
    // NOTE: We rely on the fact that compilation units change monotonously,
    // i.e. that we can only add new compilation units, but not remove them.
    if (cachedDatabaseKey != recomputeCachedDatabaseKey()) {
      val database = {
        var unmappedNames = ""
        val units = compilerUnits ++ adhocUnits
        val databases = units.map(unit => {
          try unit.toDatabase
          catch {
            case ex: Exception if ex.getMessage.startsWith("Unmapped names in") =>
              unmappedNames += (ex.getMessage + EOL)
              Database()
          }
        })
        if (unmappedNames != "") sys.error(unmappedNames.trim)
        val names = databases.flatMap(_.names).toMap
        val messages = databases.flatMap(_.messages)
        val denotations = databases.flatMap(_.denotations).toMap
        Database(names, messages, denotations)
      }
      cachedDatabaseKey = recomputeCachedDatabaseKey()
      cachedDatabase = database
    }
    cachedDatabase
  }

  def typecheck(tree: Tree): Tree = {
    if (isUnpositioned(tree)) {
      def typecheckRoot(tree: Tree): Tree = {
        tree.parent match {
          case Some(parent) =>
            val index = parent.children.indexOf(tree)
            val parent1 = typecheckRoot(parent)
            parent1.children(index)
          case _ =>
            def wrapAndTypecheck(member: Stat): Tree = {
              // NOTE: We stringify and reparse to obtain unique positions.
              // That's done because semantic APIs don't work without positions.
              val objectName = Term.fresh("scalahost$")
              val wrapper = q"object $objectName { $member }".syntax.parse[Source].get
              val addr = scala.meta.semantic.v1.Address.Snippet(wrapper.syntax)
              minputMap(wrapper.pos.input) = addr
              val member1 = wrapper.children(0).children(1).children(1)

              val abstractFile = new GVirtualFile(randomUUID.toString)
              gfileMap(abstractFile) = addr
              val sourceFile = new GBatchSourceFile(abstractFile, wrapper.syntax)
              val unit = new g.CompilationUnit(sourceFile)
              val oldReporter = g.reporter
              val reporter = new GStoreReporter()
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
                val member = q"val ${Pat.fresh("scalahost$")} = { $tree }"
                val member1 = wrapAndTypecheck(member)
                member1.children(1).children(0)
              case tree: Type =>
                val member = q"type ${Type.fresh("scalahost$")} = $tree"
                val member1 = wrapAndTypecheck(member)
                member1.children(1)
              case _ =>
                val what = "quasiquotes that are neither terms nor types"
                val restriction = s"implementation restriction: semantic API doesn't support $what"
                throw new SemanticException(tree.pos, restriction)
            }
        }
      }
      typecheckRoot(tree)
    } else {
      var message =
        s"implementation restriction: semantic API doesn't support this abstract syntax tree.$EOL"
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
