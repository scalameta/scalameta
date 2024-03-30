package scala.meta.tests.semanticdb

class Issue2024Suite extends SemanticdbSuite {
  override def munitIgnore: Boolean = !ScalaVersion.isSupported(minimal212 = 14, minimal213 = 5)

  // fixed in >=2.13.5 or >=2.12.14
  // https://github.com/scalameta/scalameta/issues/2024
  // https://github.com/scala/scala/pull/9426
  targeted(
    """|package a
       |object root {
       |  object impl
       |  val f: <<impl>>.type => Unit = {
       |    case _: <<impl>>.type => ()
       |  }
       |  f(<<impl>>)
       |}
       |""".stripMargin,
    (_, impl1, impl2, impl3) => {
      assertEquals(impl1, "a/root.impl.")
      assertEquals(impl2, "a/root.impl.")
      assertEquals(impl3, "a/root.impl.")
    }
  )
}
