package example

object Issue1872/*<=example.Issue1872.*/ {

  val List/*=>scala.package.List.*/(a1_1/*<=example.Issue1872.a1_1.*/) = List/*=>scala.package.List.*/(1)

  val a2_1/*<=example.Issue1872.a2_1.*/, List/*=>scala.package.List.*/(/*a2*/ a2_2/*<=local1*/) = List/*=>scala.package.List.*/(1, 2)
  val b2_1/*<=example.Issue1872.b2_1.*/, List/*=>scala.package.List.*/(/*b2*/ b2_2/*<=local2*/: Int/*=>scala.Int#*/) = List/*=>scala.package.List.*/(1, 2)
  val c2_1/*<=example.Issue1872.c2_1.*/, List/*=>scala.package.List.*/(/*c2*/ c2_2/*<=local3*/ @ _) = List/*=>scala.package.List.*/(1, 2)

  val a3_1/*<=example.Issue1872.a3_1.*/, List/*=>scala.package.List.*/(a3_2/*<=example.Issue1872.a3_2.*/, a3_3/*<=example.Issue1872.a3_3.*/) = List/*=>scala.package.List.*/(1, 2, 3)

}
