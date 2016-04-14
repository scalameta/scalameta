package scala.meta.tests
package tokens

// TODO: reenable this test when performance levels become acceptable
//
// import java.net._
// import java.io._
// import org.scalatest._
// import scala.util._
// import scala.meta._
// import scala.meta.dialects.Scala211
//
// class InferAndReparseSuite extends InferSuite {
//   var dir = new File(new File(System.getProperty("???")).getAbsolutePath)
//   def isProjectRoot(dir: File) = dir != null && new File(dir.getAbsolutePath + File.separatorChar + "project" + File.separatorChar + "build.scala").exists
//   while (dir != null && !isProjectRoot(dir)) dir = dir.getParentFile
//   test("ProjectDir (" + dir.getAbsolutePath + ")")(assert(isProjectRoot(dir)))
//
//   // Avoiding XML literals
//   val ignoredFiles = List("build.scala")
//
//   def loop(dir: File): Unit = {
//     def linePositionTest(src: File): Unit = {
//       test("Testing synthetic tokens for file " + src.getAbsolutePath) {
//         assert(src.exists)
//         val codec = scala.io.Codec(java.nio.charset.Charset.forName("UTF-8"))
//         val content = scala.io.Source.fromFile(src)(codec).mkString
//         val parsed = src.parse[Source]
//         val transformed = forceInferAll(parsed)
//         val newCode = transformed.tokens.map(_.show[Syntax]).mkString
//         Try(newCode.parse[Source]) match {
//           case Success(reparsed) =>
//             /* Parsing and re-comparing output. It should have reached a fixed point. */
//             val newNewCode = forceInferAll(reparsed).tokens.map(_.show[Syntax]).mkString
//             if (newCode != newNewCode) List(content, newCode, newNewCode).foreach(println)
//             assert(newCode == newNewCode)
//           case Failure(err) =>
//             printCodes(content, newCode)(Some(err))
//             fail
//         }
//       }
//     }
//     dir.listFiles.filter(_.isFile).filter(_.getName.endsWith(".scala")).filter(f => !ignoredFiles.contains(f.getName)).map(linePositionTest(_))
//     dir.listFiles.filter(_.isDirectory).map(loop(_))
//   }
//   loop(dir)
// }
