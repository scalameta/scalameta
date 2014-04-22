/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author  Martin Odersky
 */
package cbc.parser

abstract class Change
case class Insertion(text: String) extends Change
case class Deletion(nchars: Int) extends Change

