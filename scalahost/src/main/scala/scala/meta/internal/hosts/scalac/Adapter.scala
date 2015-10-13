package scala.meta
package internal.hosts.scalac

import scala.reflect.{classTag, ClassTag}
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.{meta => m}
import scala.meta.internal.hosts.scalac.contexts.{Compiler => Compiler}
import scala.meta.internal.hosts.scalac.contexts.{Adapter => AdapterImpl}
import scala.tools.nsc.Global

object Adapter {
  def apply[G <: Global](global: G): Adapter[G] = {
    new AdapterImpl[G](global) {
      override def toString = s"Adapter($global)"
    }
  }
}

// TODO: We can probably expand this to interface with any Universe
// not just with scala.tools.nsc.Global. How necessary is this, though?
trait Adapter[G <: Global] extends Context {
  self: AdapterImpl[G] =>

  val g: G

  object conversions {
    implicit class ScalahostAdapterReflectTree(gtree: g.Tree) {
      // TODO: make this work
      // def toMeta[T <: m.Tree : ClassTag](implicit ev: T OrElse m.Tree): ev.T = {
      //   implicit val tag: ClassTag[ev.T] = ev.tag
      //   self.toMtree[ev.T](gtree)
      // }
      // NOTE: can't forceTypechecked here, because gtree may be unattributed
      // and then mtree will end up unattributed as well
      def toMeta: m.Tree = self.toMtree[m.Tree](gtree)
    }
    implicit class ScalahostAdapterReflectType(gtype: g.Type) {
      // TODO: make this work
      // def toMeta[T <: m.Type.Arg : ClassTag](implicit ev: T OrElse m.Type.Arg): ev.T = {
      //   implicit val tag: ClassTag[ev.T] = ev.tag
      //   if (classOf[m.Type].isAssignableFrom(tag.runtimeClass)) self.toMtype(gtype).require[ev.T]
      //   else self.toMtypeArg(gtype).require[ev.T]
      // }
      def toMeta: m.Type.Arg = self.toMtypeArg(gtype).forceTypechecked
    }
    implicit class ScalahostAdapterReflectSymbol(gsym: g.Symbol) {
      // TODO: make this work
      // def toMeta[T <: m.Member](gpre: g.Type)(implicit ev: T OrElse m.Member): ev.T = {
      //   implicit val tag: ClassTag[ev.T] = ev.tag
      //   self.toMmember(gsym, gpre).require[ev.T]
      // }
      def toMeta(gpre: g.Type): m.Member = self.toMmember(gsym, gpre).forceTypechecked
    }
    implicit class ScalahostAdapterReflectAnnotation(gannot: g.AnnotationInfo) {
      def toMeta: m.Mod.Annot = self.toMannot(gannot).forceTypechecked
    }
    implicit class ScalahostAdapterMetaTree(mtree: m.Tree) {
      def toReflect: g.Tree = self.toGtree(mtree).asInstanceOf[g.Tree]
    }
    implicit class ScalahostAdapterMetaType(mtpe: m.Type.Arg) {
      def toReflect: g.Type = self.toGtype(mtpe).asInstanceOf[g.Type]
    }
    implicit class ScalahostAdapterMetaName(mname: m.Name) {
      def toReflect: Seq[g.Symbol] = self.toGsymbols(mname).asInstanceOf[Seq[g.Symbol]]
    }
    implicit class ScalahostAdapterMetaMember(mmember: m.Member) {
      def toReflect: Seq[g.Symbol] = self.toGsymbols(mmember).asInstanceOf[Seq[g.Symbol]]
    }
  }

  private[meta] def toMtree[T <: m.Tree : ClassTag](gtree: g.Tree): T
  private[meta] def toMtype(gtpe: g.Type): m.Type
  private[meta] def toMtypeArg(gtpe: g.Type): m.Type.Arg
  private[meta] def toMmember(gsym: g.Symbol, gpre: g.Type): m.Member
  private[meta] def toMannot(gannot: g.AnnotationInfo): m.Mod.Annot
  private[meta] def toGtree(mtree: m.Tree): g.Tree
  private[meta] def toGtype(mtpe: m.Type.Arg): g.Type
  private[meta] def toGsymbols(mname: m.Name): Seq[g.Symbol]
  private[meta] def toGsymbols(mmember: m.Member): Seq[g.Symbol]
}
