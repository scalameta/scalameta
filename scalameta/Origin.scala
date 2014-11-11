package scala.meta

import org.scalameta.adt._
import org.scalameta.convert._

// TODO: additional `pos: Position` field for Origin.String and Origin.File
// TODO: @leaf class Synthetic(tree: Tree) extends Origin

@root trait Origin {
  def content: Array[Char]
}

object Origin {
  @leaf object None extends Origin {
    lazy val content = new Array[Char](0)
  }
  @leaf class String(s: scala.Predef.String) extends Origin {
    lazy val content = s.toArray
  }
  @leaf class File(f: java.io.File) extends Origin {
    lazy val content = scala.io.Source.fromFile(f).mkString.toArray
  }
  object File {
    def apply(path: Predef.String): Origin.File = Origin.File(new java.io.File(path))
  }
  implicit val stringToOrigin: Convert[scala.Predef.String, Origin] = Convert.apply(Origin.String(_))
  implicit val fileToOrigin: Convert[java.io.File, Origin] = Convert.apply(Origin.File(_))
}
