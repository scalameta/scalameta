package scala.meta
package internal.hosts.scalac
package contexts

import org.scalameta.contexts._
import org.scalameta.invariants._
import scala.{Seq => _}
import scala.collection.immutable.Seq
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
    // TODO: implement this
    Nil
  }
}