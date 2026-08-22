package org.scalameta
package build

import sbt.Keys._
import sbt._

import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

/**
 * Packaging for the one npm artifact this build publishes, scalameta-parsers.
 *
 * Replaces io.chrisdavenport:sbt-npm-package, which has no sbt 2 build. All it did for us was
 * assemble four files and call `npm publish`.
 */
object NpmPackage {

  val npmPackage = taskKey[File]("Assemble the npm package under target; returns its directory")
  val npmPackagePublish = taskKey[Unit]("Publish the assembled npm package to the registry")

  /* TODO(esm): should this package ship ES modules rather than CommonJS?
   *
   * Two settings have to agree, and both say CommonJS today. The linker's ModuleKind, set on the JS
   * row in build.sbt, decides whether main.js speaks `require`/`module.exports` or
   * `import`/`export`; package.json's "type" below tells Node which of the two to expect.
   *
   * Going ESM means ModuleKind.ESModule, "type": "module", and probably an "exports" map in place
   * of the bare "main". The old objection was that CommonJS consumers then could not load the
   * package at all. That is largely gone: require() of an ESM package is unflagged and stable in
   * Node 20.19+ and 22.12+. What is left to weigh is older Node (the release workflow still pins
   * 18, which cannot), bundlers that resolve "main" only, and whether to ship both formats from one
   * package and route by "exports" condition.
   *
   * Left alone deliberately. It changes what consumers of scalameta-parsers receive, so it is a
   * decision about the published package, not about the build.
   */
  private val packageType = "commonjs"

  def settings(
      pkgName: String,
      pkgDescription: String,
      pkgHomepage: String,
      pkgRepository: String,
      pkgAuthor: String,
      pkgLicense: String,
      pkgKeywords: Seq[String],
      pkgReadme: File,
  ): Seq[Setting[_]] = Seq(
    npmPackage := {
      val dir = crossTarget.value / "npm-package"
      IO.delete(dir)
      IO.createDirectory(dir)
      // the linker already emits main.js and a main.js.map next to it
      IO.copyDirectory((Compile / fullLinkJSOutput).value, dir)
      IO.copyFile(pkgReadme, dir / "README.md")
      IO.write(
        dir / "package.json",
        packageJson(
          Seq(
            "name" -> pkgName,
            "version" -> version.value,
            "description" -> pkgDescription,
            "main" -> "main.js",
            "type" -> packageType,
            "homepage" -> pkgHomepage,
            "repository" -> pkgRepository,
            "author" -> pkgAuthor,
            "license" -> pkgLicense,
          ),
          pkgKeywords,
        ),
      )
      streams.value.log.info(s"assembled npm package $pkgName in $dir")
      dir
    },
    npmPackagePublish := {
      val dir = npmPackage.value
      // npm expands ${NPM_TOKEN} when it reads this, so the token never reaches disk
      IO.write(dir / ".npmrc", "//registry.npmjs.org/:_authToken=${NPM_TOKEN}\n")
      npm(dir, streams.value.log, "publish")
    },
  )

  private def packageJson(fields: Seq[(String, String)], keywords: Seq[String]): String = {
    val entries = fields.map { case (k, v) => s"  ${quote(k)}: ${quote(v)}" } :+
      s"  ${quote("keywords")}: [${keywords.map(quote).mkString(", ")}]"
    entries.mkString("{\n", ",\n", "\n}\n")
  }

  private def quote(str: String): String = {
    val sb = new StringBuilder("\"")
    str.foreach {
      case '"' => sb ++= "\\\""
      case '\\' => sb ++= "\\\\"
      case '\n' => sb ++= "\\n"
      case c if c < ' ' => sb ++= f"\\u$c%04x"
      case c => sb += c
    }
    sb.append('"').toString
  }

  private def npm(dir: File, log: Logger, args: String*): Unit = {
    val exe =
      if (sys.props("os.name").toLowerCase.contains("win")) Seq("cmd", "/c", "npm") else Seq("npm")
    val logger = scala.sys.process.ProcessLogger(log.info(_), log.error(_))
    val code = scala.sys.process.Process(exe ++ args, dir).!(logger)
    if (code != 0) sys.error(s"npm ${args.mkString(" ")} failed with exit code $code")
  }

}
