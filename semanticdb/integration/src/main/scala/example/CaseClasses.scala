package example

object CaseClasses1 {
  case class CClass(i: Int, j: String)

  val cclass1 = CClass(1, "")
  val cclass2 = CClass.apply(2, "")
  val cclass3 = CClass(i = 3, j = "")
  val cclass4 = CClass.apply(i = 4, j = "")
}

object CaseClasses2 {
  case class CClass(j: String)
  object CClass { def apply(i: Int): CClass = CClass(i.toString) }

  val cclass1 = CClass("")
  val cclass2 = CClass.apply("")
  val cclass3 = CClass(j = "")
  val cclass4 = CClass.apply(j = "")
  val cclass1b = CClass(1)
  val cclass2b = CClass.apply(2)
  val cclass3b = CClass(i = 3)
  val cclass4b = CClass.apply(i = 4)
}
