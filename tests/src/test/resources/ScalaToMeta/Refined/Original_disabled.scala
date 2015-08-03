trait Refined {
  def test = ((x: Int, y: Int) => Refined.create).curried
}

object Refined {
  def create = new Refined {
    val x = ???
    var y = ???
    def z(w: Int) = ???
    type T1 <: Nothing
    type T2 = Nothing
  }
}
