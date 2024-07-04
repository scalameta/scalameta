package scala.meta.tests

object TestHelpers {

  def getMessageWithExtraClue(msg: String, extraClue: String = ""): String =
    if (extraClue.isEmpty) msg else s"$msg ($extraClue)"

}
