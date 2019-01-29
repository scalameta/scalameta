package example

class ForComprehension/*example.ForComprehension#*/ {
  for {
    a/*local0*/ <- List/*scala.collection.immutable.List.*/(1)
    b/*local1*/ <- List/*scala.collection.immutable.List.*/(1)
    if b/*local1*/ >/*scala.Int#`>`(+3).*/ 1
    c/*local3*/ = a/*local0*/ +/*scala.Int#`+`(+4).*/ b/*local1*/
  } yield (a/*local0*/, b/*local1*/, c/*local3*/)
}
