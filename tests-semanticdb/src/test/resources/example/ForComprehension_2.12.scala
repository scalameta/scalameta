package example

class ForComprehension/*<=example.ForComprehension#*/ {
  for {
    a/*<=local0*/ <- List/*=>scala.collection.immutable.List.*/(1)
    b/*<=local1*/ <- List/*=>scala.collection.immutable.List.*/(1)
    if b/*=>local1*/ >/*=>scala.Int#`>`(+3).*/ 1
    c/*<=local3*/ = a/*=>local0*/ +/*=>scala.Int#`+`(+4).*/ b/*=>local1*/
  } yield (a/*=>local0*/, b/*=>local1*/, c/*=>local3*/)
  for {
    a/*<=local4*/ <- List/*=>scala.collection.immutable.List.*/(1)
    b/*<=local5*/ <- List/*=>scala.collection.immutable.List.*/(a/*=>local4*/)
    if (
      a/*=>local4*/,
      b/*=>local5*/
    ) ==/*=>java.lang.Object#`==`().*/ (1, 2)
    (
      c/*<=local11*/,
      d/*<=local12*/
    ) <- List/*=>scala.collection.immutable.List.*/((a/*=>local4*/, b/*=>local5*/))
    if (
      a/*=>local4*/,
      b/*=>local5*/,
      c/*=>local11*/,
      d/*=>local12*/
    ) ==/*=>java.lang.Object#`==`().*/ (1, 2, 3, 4)
    e/*<=local13*/ = (
      a/*=>local4*/,
      b/*=>local5*/,
      c/*=>local11*/,
      d/*=>local12*/
    )
    if e/*=>local13*/ ==/*=>java.lang.Object#`==`().*/ (1, 2, 3, 4)
    f/*<=local10*/ <- List/*=>scala.collection.immutable.List.*/(e/*=>local13*/)
  } yield {
    (
      a/*=>local4*/,
      b/*=>local5*/,
      c/*=>local11*/,
      d/*=>local12*/,
      e/*=>local13*/,
      f/*=>local10*/
    )
  }
  val x/*<=example.ForComprehension#x.*/: Option/*=>scala.Option#*/[(Int/*=>scala.Int#*/, Int/*=>scala.Int#*/)] = None/*=>scala.None.*/
  for {
    (_, _) <- x/*=>example.ForComprehension#x.*/
  } yield ()
}
