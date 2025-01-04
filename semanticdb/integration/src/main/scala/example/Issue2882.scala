package example

object Issue2882 {

  case class FooFailure(msg: String)
    extends Exception(s"Error: $msg")

}
