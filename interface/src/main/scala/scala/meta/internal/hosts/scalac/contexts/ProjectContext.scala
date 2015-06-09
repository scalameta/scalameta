package scala.meta
package internal.hosts.scalac
package contexts

import org.scalameta.contexts._
import org.scalameta.invariants._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.meta.semantic.{Context => ScalametaSemanticContext}
import scala.meta.projects.{Context => ScalametaProjectContext}

@context(translateExceptions = false)
class ProjectContext(sourcepath: String, classpath: String) extends ScalametaSemanticContext with ScalametaProjectContext {
  private val c = new StandaloneContext(s"-classpath $classpath")
  def dialect: Dialect = c.dialect
  def desugar(term: Term): Term = c.desugar(term)
  def tpe(term: Term): Type = c.tpe(term)
  def tpe(param: Term.Param): Type.Arg = c.tpe(param)
  def defns(ref: Ref): Seq[Member] = c.defns(ref)
  def members(tpe: Type): Seq[Member] = c.members(tpe)
  def isSubType(tpe1: Type, tpe2: Type): Boolean = c.isSubType(tpe1, tpe2)
  def lub(tpes: Seq[Type]): Type = c.lub(tpes)
  def glb(tpes: Seq[Type]): Type = c.glb(tpes)
  def parents(tpe: Type): Seq[Type] = c.parents(tpe)
  def widen(tpe: Type): Type = c.widen(tpe)
  def dealias(tpe: Type): Type = c.dealias(tpe)
  def parents(member: Member): Seq[Member] = c.parents(member)
  def children(member: Member): Seq[Member] = c.children(member)

  // TODO: figure out how to populate resources and dependencies
  def project: Project = Project.Local(new java.io.File(sourcepath).toURI, computeSources(), Nil, Nil)
  private def computeSources(): Seq[Source] = {
    import java.io._
    import scala.tools.asm._
    import scala.meta.internal.hosts.scalac.tasty._
    val classpathEntries = classpath.split(File.pathSeparatorChar).toList
    val sources = classpathEntries.flatMap(entryPath => {
      val entryFile = new File(entryPath)
      val entrySources = mutable.ListBuffer[Source]()
      // TODO: support additional types of classpath entries, e.g. jar files
      if (entryFile.isDirectory) {
        def loop(entryFile: File): Unit = {
          def visit(entryFile: File): Unit = {
            if (entryFile.getName.endsWith(".class")) {
              val in = new FileInputStream(entryFile)
              try {
                val classReader = new ClassReader(in)
                classReader.accept(new ClassVisitor(Opcodes.ASM4) {
                  override def visitAttribute(attr: Attribute) {
                    if (attr.`type` == "TASTY") {
                      val valueField = attr.getClass.getDeclaredField("value")
                      valueField.setAccessible(true)
                      val value = valueField.get(attr).asInstanceOf[Array[Byte]]
                      entrySources ++= Source.fromTasty(value)
                    }
                    super.visitAttribute(attr)
                  }
                }, 0)
              } finally {
                in.close()
              }
            }
          }
          entryFile.listFiles.filter(_.isFile).foreach(visit)
          entryFile.listFiles.filter(_.isDirectory).foreach(loop)
        }
        loop(entryFile)
      }
      entrySources.toList
    })
    // TODO: correlate sources loaded from TASTY and sources from sourcepath
    sources
  }
}