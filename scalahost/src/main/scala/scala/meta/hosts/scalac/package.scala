// Parts of this file are originally taken from lihaoyi/ammonite:
// https://github.com/lihaoyi/Ammonite/blob/cd5de73b5601735093f4f80a775423b7a0102b37/repl/src/main/scala/ammonite/repl/IvyThing.scala

package scala.meta
package hosts

import org.apache.ivy.plugins.resolver._
import scala.{Seq => _}
import scala.collection.immutable.Seq
import scala.meta.taxonomic.{Context => ScalametaTaxonomy}
import scala.meta.internal.hosts.scalac.contexts.{Taxonomy => ScalahostTaxonomy}

package object scalac {
  lazy val defaultResolvers: Seq[DependencyResolver] = {
    def resolver(name: String) = {
      val res = new IBiblioResolver()
      res.setUsepoms(true)
      res.setM2compatible(true)
      res.setName(name)
      res.setRoot("http://repo1.maven.org/maven2/")
      res
    }
    //add duplicate resolvers with different name to make Ivy shut up
    //and stop giving `unknown resolver null` or `unknown resolver sbt-chain`
    //errors
    Seq(resolver("central"), resolver("sbt-chain"), resolver("null"))
  }

  implicit lazy val DefaultTaxonomy: ScalametaTaxonomy = new ScalahostTaxonomy(defaultResolvers: _*)
  def CustomTaxonomy(resolvers: DependencyResolver*): ScalametaTaxonomy = new ScalahostTaxonomy(resolvers: _*)
}
