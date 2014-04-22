/* NSC -- new scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */

import scala.annotation.elidable
import scala.collection.{ mutable, immutable }
import cbc.{TreeGen => InternalTreeGen}
import cbc.util._

package object cbc {
  // Members declared in cbc.FreshNames
  def currentFreshNameCreator: cbc.util.FreshNameCreator = new cbc.util.FreshNameCreator
}

