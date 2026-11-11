package scala.meta.tests.semanticdb.dotc

/** Writes the generated output of the module. */
object SaveDotc {

  def main(args: Array[String]): Unit = DotcOutput
    .write(DotcOutput.Committed, DotcRewriter(DotcOutput.Raw))

}
