package scala.meta.tests

object TestHelpers {

  def getMessageWithExtraClue(msg: String, extraClue: String = ""): String =
    if (extraClue.isEmpty) msg else s"$msg ($extraClue)"

  def getSyntax(code: => String, syntax: String = null): String = Option(syntax).getOrElse(code)

}
