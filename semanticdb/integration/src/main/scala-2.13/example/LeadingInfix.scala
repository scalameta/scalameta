package example

trait LeadingInfixTest {
  def ?=(right: Any): Unit
  def arg = 1

  def testNoLeadingInfix = {
    this ?= (arg)
  }

  def testNoComments = {
    this
    ?= (arg)
  }

  def testAttachedSlcMlcAttachedOp = {
    this // c1
    /*
      c2
     */
    ?= (arg)
  }

  def testDetachedSlcMlcAttachedOp = {
    this
    // c1
    /*
      c2
     */
    ?= (arg)
  }

  def testDetachedSlcMlcDetachedOp = {
    this
    // c1
    /*
      c2
     */
    ?= (arg)
  }

  def testAttachedMlcDetachedSlcOp = {
    this /*
      c2
     */
    // c1
    ?= (arg)
  }

  def testDetachedMlcDetachedSlcOp = {
    this
    /*
      c2
     */
    // c1
    ?= (arg)
  }

}
