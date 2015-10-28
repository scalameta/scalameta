package Simple3

class C {
  def loop(x: Int): Int = {
    def helper(x: Int): String = x.toString
    if (helper(x) == "0") 0
    else loop(x - 1)
  }
  loop(42)
}