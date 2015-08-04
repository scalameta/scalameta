package scala.meta
package internal.hosts.scalac
package contexts

import org.scalameta.contexts._
import org.scalameta.invariants._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.collection.mutable
import scala.meta.{ScalahostProjectContext => ProjectContextApi}
import scala.meta.internal.{ast => impl}
import scala.meta.internal.hosts.scalac.converters.mergeTrees

@context(translateExceptions = false)
class ProjectContext(sourcepath: String, classpath: String) extends ProjectContextApi {
  private implicit val c = new StandaloneContext(s"-classpath $classpath")
  private implicit val d: Dialect = c.dialect
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
    def lurk(file: File): List[File] = {
      if (file.isDirectory) {
        val shallowFiles = file.listFiles.filter(_.isFile).toList
        val recursiveFiles = file.listFiles.filter(_.isDirectory).flatMap(lurk).toList
        shallowFiles ++ recursiveFiles
      } else {
        Nil
      }
    }
    val sourcepathSources = lurk(new File(sourcepath)).filter(_.getName.endsWith(".scala")).map(_.parse[Source])
    val classpathEntries = classpath.split(File.pathSeparatorChar).toList
    val classpathSources = classpathEntries.flatMap(entryPath => {
      val entryClasses = lurk(new File(entryPath)).filter(_.getName.endsWith(".class"))
      val entrySources = mutable.ListBuffer[Source]()
      entryClasses.foreach(entryClass => {
        val in = new FileInputStream(entryClass)
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
      })
      entrySources.toList
    })
    def mergeSources(trees: (Source, Source)) = {
      val syntactic = trees._1.require[impl.Source]
      val semantic = trees._2.require[impl.Source]
      val merged = scala.meta.internal.hosts.scalac.converters.mergeTrees(syntactic, semantic)
      merged.require[Source]
    }
    sourcepathSources.zip(classpathSources).map(mergeSources)
  }
}