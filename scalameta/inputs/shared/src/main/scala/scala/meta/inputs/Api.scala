package org.langmeta.internal.inputs {
  trait Api extends org.langmeta.inputs.Api
  trait Aliases extends org.langmeta.inputs.Aliases
}

package scala.meta.inputs {
  private[meta] trait Api extends org.langmeta.internal.inputs.Api {
    import java.nio.charset._
    import scala.meta.common._
    import org.langmeta.io.AbsolutePath
    implicit val charsToInput: Convert[Array[Char], Input] = Convert(chars => Input.String(new scala.Predef.String(chars)))
    implicit val stringToInput: Convert[scala.Predef.String, Input] = Convert(Input.String(_))
    implicit def streamToInput[T <: java.io.InputStream]: Convert[T, Input] = Convert(is => Input.Stream(is, Charset.forName("UTF-8")))
    // NOTE: fileToInput is lazy to avoid linking errors in Scala.js
    implicit lazy val fileToInput: Convert[java.io.File, Input] = Convert(Input.File.apply)
    implicit lazy val nioPathToInput: Convert[java.nio.file.Path, Input] = Convert(Input.File.apply)
    implicit lazy val absolutePathToInput: Convert[AbsolutePath, Input] = Convert(Input.File.apply)
  }

  private[meta] trait Aliases extends org.langmeta.internal.inputs.Aliases {
  }
}
