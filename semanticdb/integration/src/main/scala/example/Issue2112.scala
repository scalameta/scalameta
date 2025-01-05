package example

object Issue2112 {
  val myNumber = 3
  val myList = List(1, 2, 3)
  myNumber :: myList
  myNumber :: 5 :: Nil

  def foo(myNumber: Int) = {
    myNumber :: myList
    myNumber :: 5 :: Nil
  }
}
