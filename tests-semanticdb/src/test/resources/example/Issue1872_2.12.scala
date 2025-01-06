package example

object Issue1872/*<=example.Issue1872.*/ {

  val List/*=>scala.collection.immutable.List.*/(a1_1/*<=example.Issue1872.a1_1.*/) = List/*=>scala.collection.immutable.List.*/(1)

  val a2_1/*<=example.Issue1872.a2_1.*/, List/*=>scala.collection.immutable.List.*/(/*a2*/ a2_2/*<=example.Issue1872.a2_2.*/) = List/*=>scala.collection.immutable.List.*/(1, 2)
  val b2_1/*<=example.Issue1872.b2_1.*/, List/*=>scala.collection.immutable.List.*/(/*b2*/ b2_2/*<=example.Issue1872.b2_2.*/: Int/*=>scala.Int#*/) = List/*=>scala.collection.immutable.List.*/(1, 2)
  val c2_1/*<=example.Issue1872.c2_1.*/, List/*=>scala.collection.immutable.List.*/(/*c2*/ c2_2/*<=example.Issue1872.c2_2.*/ @ _) = List/*=>scala.collection.immutable.List.*/(1, 2)

  val a3_1/*<=example.Issue1872.a3_1.*/, List/*=>scala.collection.immutable.List.*/(a3_2/*<=example.Issue1872.a3_2.*/, a3_3/*<=example.Issue1872.a3_3.*/) = List/*=>scala.collection.immutable.List.*/(1, 2, 3)

}
