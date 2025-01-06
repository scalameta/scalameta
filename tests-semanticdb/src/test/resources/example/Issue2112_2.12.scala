package example

object Issue2112/*<=example.Issue2112.*/ {
  val myNumber/*<=example.Issue2112.myNumber.*/ = 3
  val myList/*<=example.Issue2112.myList.*/ = List/*=>scala.collection.immutable.List.*/(1, 2, 3)
  myNumber/*=>example.Issue2112.myNumber.*/ ::/*=>scala.collection.immutable.List#`::`().*/ myList/*=>example.Issue2112.myList.*/
  myNumber/*=>example.Issue2112.myNumber.*/ ::/*=>scala.collection.immutable.List#`::`().*/ 5 ::/*=>scala.collection.immutable.List#`::`().*/ Nil/*=>scala.collection.immutable.Nil.*/

  def foo/*<=example.Issue2112.foo().*/(myNumber/*<=example.Issue2112.foo().(myNumber)*/: Int/*=>scala.Int#*/) = {
    myNumber/*=>example.Issue2112.foo().(myNumber)*/ ::/*=>scala.collection.immutable.List#`::`().*/ myList/*=>example.Issue2112.myList.*/
    myNumber/*=>example.Issue2112.foo().(myNumber)*/ ::/*=>scala.collection.immutable.List#`::`().*/ 5 ::/*=>scala.collection.immutable.List#`::`().*/ Nil/*=>scala.collection.immutable.Nil.*/
  }
}
