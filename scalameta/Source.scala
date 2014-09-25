package scala.meta
import org.scalameta.adt._
import org.scalameta.convert._

@root trait Source { 
  def content: Array[Char] 
}
object Source {
  @leaf object None extends Source { def content = new Array[Char](0) }
  @leaf class String(s: scala.Predef.String) extends Source {
    lazy val content = s.toArray
  }
  @leaf class File(f: java.io.File) extends Source {
    lazy val content = scala.io.Source.fromFile(f).mkString.toArray
  }
  object File {
    def apply(path: Predef.String): Source.File = Source.File(new java.io.File(path))
  }
  implicit val stringToSource: Convert[scala.Predef.String, Source] = Convert.apply(Source.String(_))
  implicit val fileToSource: Convert[java.io.File, Source] = Convert.apply(Source.File(_))
}
