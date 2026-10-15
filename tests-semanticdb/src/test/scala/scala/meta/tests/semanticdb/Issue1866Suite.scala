package scala.meta.tests.semanticdb

// https://github.com/scalameta/scalameta/issues/1866
class Issue1866Suite extends SemanticdbSuite {

  // implicit conversions (#1866)
  synthetics(
    """|package w
       |import scala.language.implicitConversions
       |object o { implicit def viaDef(s: String): Option[Int] = None }
       |class C {
       |  implicit val viaVal: Int => Option[Int] = _ => None
       |  implicit def local(c: Char): Option[Int] = None
       |  implicit class Rich(b: Boolean) { def twice = b }
       |  val a: Option[Int] = 1
       |  val b: Option[Int] = 'c'
       |  val c = true.twice
       |  locally { import o._; val d: Option[Int] = "s" }
       |}
       |""".stripMargin,
    """|[7:23..7:24): 1 => viaVal.apply(*)
       |  viaVal => w/C#viaVal.
       |  apply => scala/Function1#apply().
       |[8:23..8:26): 'c' => local(*)
       |  local => w/C#local().
       |[9:10..9:14): true => Rich(*)
       |  Rich => w/C#Rich().
       |[10:2..10:9): locally => *[Unit]
       |  Unit => scala/Unit#
       |[10:45..10:48): "s" => viaDef(*)
       |  viaDef => w/o.viaDef().
       |""".stripMargin,
  )

}
