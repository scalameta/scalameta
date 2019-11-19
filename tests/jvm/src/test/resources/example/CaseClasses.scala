package example

object CaseClasses1/*<=example.CaseClasses1.*/ {
  case class CClass/*<=example.CaseClasses1.CClass#*/(i/*<=example.CaseClasses1.CClass#i.*/: Int/*=>scala.Int#*/, j/*<=example.CaseClasses1.CClass#j.*/: String/*=>scala.Predef.String#*/)

  val cclass1/*<=example.CaseClasses1.cclass1.*/ = CClass/*=>example.CaseClasses1.CClass.*/(1, "")
  val cclass2/*<=example.CaseClasses1.cclass2.*/ = CClass/*=>example.CaseClasses1.CClass.*/.apply/*=>example.CaseClasses1.CClass.apply().*/(2, "")
  val cclass3/*<=example.CaseClasses1.cclass3.*/ = CClass/*=>example.CaseClasses1.CClass.*/(i/*=>example.CaseClasses1.CClass#i.*/ = 3, j/*=>example.CaseClasses1.CClass#j.*/ = "")
  val cclass4/*<=example.CaseClasses1.cclass4.*/ = CClass/*=>example.CaseClasses1.CClass.*/.apply/*=>example.CaseClasses1.CClass.apply().*/(i/*=>example.CaseClasses1.CClass#i.*/ = 4, j/*=>example.CaseClasses1.CClass#j.*/ = "")
}

object CaseClasses2/*<=example.CaseClasses2.*/ {
  case class CClass/*<=example.CaseClasses2.CClass#*/(j/*<=example.CaseClasses2.CClass#j.*/: String/*=>scala.Predef.String#*/)
  object CClass/*<=example.CaseClasses2.CClass.*/ { def apply/*<=example.CaseClasses2.CClass.apply().*/(i/*<=example.CaseClasses2.CClass.apply().(i)*/: Int/*=>scala.Int#*/): CClass/*=>example.CaseClasses2.CClass#*/ = CClass/*=>example.CaseClasses2.CClass.*/(i/*=>example.CaseClasses2.CClass.apply().(i)*/.toString/*=>scala.Any#toString().*/) }

  val cclass1/*<=example.CaseClasses2.cclass1.*/ = CClass/*=>example.CaseClasses2.CClass.*/("")
  val cclass2/*<=example.CaseClasses2.cclass2.*/ = CClass/*=>example.CaseClasses2.CClass.*/.apply/*=>example.CaseClasses2.CClass.apply(+1).*/("")
  val cclass3/*<=example.CaseClasses2.cclass3.*/ = CClass/*=>example.CaseClasses2.CClass.*/(j/*=>example.CaseClasses2.CClass#j.*/ = "")
  val cclass4/*<=example.CaseClasses2.cclass4.*/ = CClass/*=>example.CaseClasses2.CClass.*/.apply/*=>example.CaseClasses2.CClass.apply(+1).*/(j/*=>example.CaseClasses2.CClass#j.*/ = "")
  val cclass1b/*<=example.CaseClasses2.cclass1b.*/ = CClass/*=>example.CaseClasses2.CClass.*/(1)
  val cclass2b/*<=example.CaseClasses2.cclass2b.*/ = CClass/*=>example.CaseClasses2.CClass.*/.apply/*=>example.CaseClasses2.CClass.apply().*/(2)
  val cclass3b/*<=example.CaseClasses2.cclass3b.*/ = CClass/*=>example.CaseClasses2.CClass.*/(i/*=>example.CaseClasses2.CClass.apply().(i)*/ = 3)
  val cclass4b/*<=example.CaseClasses2.cclass4b.*/ = CClass/*=>example.CaseClasses2.CClass.*/.apply/*=>example.CaseClasses2.CClass.apply().*/(i/*=>example.CaseClasses2.CClass.apply().(i)*/ = 4)
}
