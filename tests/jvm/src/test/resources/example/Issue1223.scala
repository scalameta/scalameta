package example

object inAppl/*<=example.inAppl.*/ {
  val a/*<=example.inAppl.a.*/ = Seq/*=>scala.collection.Seq.*/(1, 2, 3)
  val b/*<=example.inAppl.b.*/ = Seq/*=>scala.collection.Seq.*/("a", "b", "c")
  (a/*=>example.inAppl.a.*/ zip/*=>scala.collection.IterableLike#zip().*/ b/*=>example.inAppl.b.*/)
}
