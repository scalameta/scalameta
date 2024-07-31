package example

trait LeadingInfixTest/*<=example.LeadingInfixTest#*/ {
  def ?=/*<=example.LeadingInfixTest#`?=`().*/(right/*<=example.LeadingInfixTest#`?=`().(right)*/: Any/*=>scala.Any#*/): Unit/*=>scala.Unit#*/
  def arg/*<=example.LeadingInfixTest#arg().*/ = 1

  def testNoLeadingInfix/*<=example.LeadingInfixTest#testNoLeadingInfix().*/ = {
    this ?=/*=>example.LeadingInfixTest#`?=`().*/ (arg/*=>example.LeadingInfixTest#arg().*/)
  }

  def testNoComments/*<=example.LeadingInfixTest#testNoComments().*/ = {
    this
    ?=/*=>example.LeadingInfixTest#`?=`().*/ (arg/*=>example.LeadingInfixTest#arg().*/)
  }

  def testAttachedSlcMlcAttachedOp/*<=example.LeadingInfixTest#testAttachedSlcMlcAttachedOp().*/ = {
    this // c1
    /*
      c2
     */
    ?=/*=>example.LeadingInfixTest#`?=`().*/ (arg/*=>example.LeadingInfixTest#arg().*/)
  }

  def testDetachedSlcMlcAttachedOp/*<=example.LeadingInfixTest#testDetachedSlcMlcAttachedOp().*/ = {
    this
    // c1
    /*
      c2
     */
    ?=/*=>example.LeadingInfixTest#`?=`().*/ (arg/*=>example.LeadingInfixTest#arg().*/)
  }

  def testDetachedSlcMlcDetachedOp/*<=example.LeadingInfixTest#testDetachedSlcMlcDetachedOp().*/ = {
    this
    // c1
    /*
      c2
     */
    ?=/*=>example.LeadingInfixTest#`?=`().*/ (arg/*=>example.LeadingInfixTest#arg().*/)
  }

  def testAttachedMlcDetachedSlcOp/*<=example.LeadingInfixTest#testAttachedMlcDetachedSlcOp().*/ = {
    this /*
      c2
     */
    // c1
    ?=/*=>example.LeadingInfixTest#`?=`().*/ (arg/*=>example.LeadingInfixTest#arg().*/)
  }

  def testDetachedMlcDetachedSlcOp/*<=example.LeadingInfixTest#testDetachedMlcDetachedSlcOp().*/ = {
    this
    /*
      c2
     */
    // c1
    ?=/*=>example.LeadingInfixTest#`?=`().*/ (arg/*=>example.LeadingInfixTest#arg().*/)
  }

}
