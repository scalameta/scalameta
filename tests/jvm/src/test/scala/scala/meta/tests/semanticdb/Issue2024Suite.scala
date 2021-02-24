package scala.meta.tests.semanticdb

class Issue2024Suite extends SemanticdbSuite {
  override def munitIgnore: Boolean = !isSupported(scala.util.Properties.versionNumberString)

  private def isSupported(version: String): Boolean = {
    val Array(major, minor, patch) =
      version.replaceAll("(-|\\+).+$", "").split('.').map(_.toInt)
    (major, minor) match {
      case (2, 13) => patch >= 5
      case (2, 12) => patch >= 14
      case _ => false
    }
  }

  // fixed in >=2.13.5 or >=2.12.14
  // https://github.com/scalameta/scalameta/issues/2024
  // https://github.com/scala/scala/pull/9426
  targeted(
    """package a
      |object root {
      |  object impl
      |  val f: <<impl>>.type => Unit = {
      |    case _: <<impl>>.type => ()
      |  }
      |  f(<<impl>>)
      |}
    """.stripMargin,
    (_, impl1, impl2, impl3) => {
      assert(impl1 == "a/root.impl.")
      assert(impl2 == "a/root.impl.")
      assert(impl3 == "a/root.impl.")
    }
  )
}
