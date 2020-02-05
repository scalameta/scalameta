package docs
import java.net.URL
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import java.nio.file.StandardOpenOption
import java.util.concurrent.TimeUnit
import java.util.regex.Pattern
import scala.meta.internal.io.InputStreamIO
import scala.util.control.NonFatal

case class MarkdownFile(
    title: String,
    id: String,
    filename: String,
    sidebarLabel: String,
    url: String,
    postProcess: String => String = identity
)

object ScalametaDocs {
  def download(out: Path): Unit = {
    println("Downloading scalameta docs...")
    val start = System.nanoTime()
    files.foreach(file => downloadFile(out, file))
    val end = System.nanoTime()
    val elapsed = TimeUnit.NANOSECONDS.toMillis(end - start)
    println(s"Done! (${elapsed}ms)")
  }

  // TODO: https://github.com/scalameta/scalameta/pull/1764
  // val root = "https://raw.githubusercontent.com/scalameta/scalameta/master"
  val orgRepo = "scalameta/scalameta"
  val branch = "master"
  val root =
    s"https://raw.githubusercontent.com/$orgRepo/$branch"
  val repoPath =
    s"https://github.com/$orgRepo/blob/$branch"

  def applyReplacements(
      string: String,
      replacements: (Pattern, String)*): String =
    replacements.iterator.foldLeft(string) {
      case (nextString, (nextPattern, replacement)) =>
        nextPattern.matcher(nextString).replaceAll(replacement)
    }

  def semanticdbProto = Pattern.compile("\\(semanticdb.proto\\)")
  def semanticdbProto2 =
    Pattern.compile("\\[semanticdb.proto\\]: semanticdb.proto")

  def files = List(
    MarkdownFile(
      title = "Quasiquotes Specification",
      id = "quasiquotes",
      filename = "trees/quasiquotes.md",
      sidebarLabel = "Quasiquotes",
      url = s"$root/notes/quasiquotes.md"
    ),
    MarkdownFile(
      title = "Tree Examples",
      id = "examples",
      filename = "trees/examples.md",
      sidebarLabel = "Examples",
      url = s"$root/notes/trees.md",
      postProcess = { notes =>
        val header = Pattern.compile("^# Tree Examples.*")
        applyReplacements(
          notes,
          header -> ""
        )
      }
    ),
    MarkdownFile(
      title = "SemanticDB Guide",
      id = "guide",
      filename = "semanticdb/guide.md",
      sidebarLabel = "Guide",
      url = s"$root/semanticdb/semanticdb3/guide.md",
      postProcess = { guide =>
        val toc =
          Pattern.compile(
            "- \\[Installation.*\\  \\* \\[Metals\\]\\(#metals\\)",
            Pattern.DOTALL)
        val header = Pattern.compile("^# SemanticDB.*")
        val semanticdb3 = Pattern.compile("\\(semanticdb3.md\\)")
        applyReplacements(
          guide,
          toc -> "",
          header -> "",
          semanticdb3 -> "(specification.html)",
          semanticdbProto -> s"($repoPath/semanticdb/semanticdb/semanticdb.proto)"
        )
      }
    ),
    MarkdownFile(
      title = "SemanticDB Specification",
      id = "specification",
      filename = "semanticdb/specification.md",
      sidebarLabel = "Specification",
      url = s"$root/semanticdb/semanticdb3/semanticdb3.md",
      postProcess = { spec =>
        val prelude =
          """
            |SemanticDB is a data model for semantic information such as symbols and types about 
            |programs in Scala and other languages. SemanticDB decouples production and consumption 
            |of semantic information, establishing documented means for communication between tools.
            |""".stripMargin
        val body =
          spec.lines.dropWhile(!_.startsWith("## Motivation")).mkString("\n")
        applyReplacements(
          prelude + body,
          semanticdbProto2 -> s"[semanticdb.proto]: $repoPath/semanticdb/semanticdb/semanticdb.proto",
          semanticdbProto -> s"($repoPath/semanticdb/semanticdb/semanticdb.proto)"
        )
      }
    )
  )

  private def downloadFile(out: Path, md: MarkdownFile): Unit = {
    try {
      val uri = new URL(md.url)
      val filename = Paths.get(md.filename)
      val file = out.resolve(filename)
      val in = uri.openStream()
      val bytes =
        try InputStreamIO.readBytes(in)
        finally in.close()
      val postprocessed = md.postProcess(new String(bytes))
      val frontMatter =
        s"""---
           |id: ${md.id}
           |title: ${md.title}
           |sidebar_label: ${md.sidebarLabel}
           |---
           |""".stripMargin
      Files.createDirectories(file.getParent)
      Files.write(
        file,
        frontMatter.getBytes(),
        StandardOpenOption.TRUNCATE_EXISTING,
        StandardOpenOption.CREATE
      )
      Files.write(
        file,
        postprocessed.getBytes(),
        StandardOpenOption.APPEND
      )
    } catch {
      case NonFatal(e) =>
        println("error: " + e.getMessage)
    }

  }
}
