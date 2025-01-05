package example

object Issue2112/*<=example.Issue2112.*/ {
  val myNumber/*<=example.Issue2112.myNumber.*/ = 3
  val myList/*<=example.Issue2112.myList.*/ = List/*=>scala.package.List.*/(1, 2, 3)
  myNumber/*=>local0*/ ::/*=>scala.collection.immutable.List#`::`().*/ myList/*=>example.Issue2112.myList.*/
  myNumber/*=>local1*/ ::/*=>scala.collection.immutable.List#`::`().*/ 5 ::/*=>scala.collection.immutable.List#`::`().*/ Nil/*=>scala.package.Nil.*/

  def foo/*<=example.Issue2112.foo().*/(myNumber/*<=example.Issue2112.foo().(myNumber)*/: Int/*=>scala.Int#*/) = {
    myNumber/*=>local2*/ ::/*=>scala.collection.immutable.List#`::`().*/ myList/*=>example.Issue2112.myList.*/
    myNumber/*=>local3*/ ::/*=>scala.collection.immutable.List#`::`().*/ 5 ::/*=>scala.collection.immutable.List#`::`().*/ Nil/*=>scala.package.Nil.*/
  }
}
