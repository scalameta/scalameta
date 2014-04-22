/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

import scala.annotation.elidable
import scala.collection.{ mutable, immutable }
import cbc.{TreeGen => InternalTreeGen}
import cbc.util._

package object cbc {
  val reporter: cbc.reporters.Reporter = new cbc.reporters.ConsoleReporter

  // Members declared in cbc.FreshNames
  def currentFreshNameCreator: cbc.util.FreshNameCreator = new cbc.util.FreshNameCreator

  def inform(msg: String): Unit = ()
  def abort(msg: String): Nothing = throw new Exception("abort: $msg")

  /** Called from parser, which signals hereby that a method definition has been parsed. */
  def signalParseProgress(pos: Position) {}
}

