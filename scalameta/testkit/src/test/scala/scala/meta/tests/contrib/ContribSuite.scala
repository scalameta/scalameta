package scala.meta.tests
package contrib

import scala.meta.testkit.Corpus
import scala.util.Random

object ContribSuite {
  private val rand = Random.nextInt(1000)
  // println("Random seed: " + rand)
  val corpus = Corpus
    .files(Corpus.fastparse)
    .drop(rand)
    .take(100)
    .toBuffer
}
