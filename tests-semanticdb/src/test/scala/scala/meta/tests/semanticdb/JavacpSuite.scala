package scala.meta.tests.semanticdb

import scala.meta.internal.classpath.ClasspathIndex
import scala.meta.internal.javacp.Javacp
import scala.meta.internal.semanticdb.Scala._
import scala.meta.internal.semanticdb.SymbolInformation.Property._
import scala.meta.internal.semanticdb._
import scala.meta.io.Classpath

import scala.tools.asm.Opcodes
import scala.tools.asm.tree.{ClassNode, MethodNode}

class JavacpSuite extends JavacpSuiteBase {

  checkOrder(
    "methods",
    "com/javacp/MetacJava#",
    _.desc.value == "overload",
    List(
      "com/javacp/MetacJava#overload().",
      "com/javacp/MetacJava#overload(+2).",
      "com/javacp/MetacJava#overload(+1).",
    ),
  )

  checkOrder(
    "fields",
    "com/javacp/Test#",
    s => s.desc.value == "Int" || s.desc.value == "Long" || s.desc.value == "Float",
    List("com/javacp/Test#Int.", "com/javacp/Test#Long.", "com/javacp/Test#Float."),
  )

  private val modifiers = "com/javacp/Modifiers#"

  checkProperties("native method", modifiers + "nativeMethod().", List(NATIVE))
  checkProperties("synchronized method", modifiers + "synchronizedMethod().", List(SYNCHRONIZED))
  checkProperties("transient field", modifiers + "transientField.", List(TRANSIENT))
  checkProperties("volatile field", modifiers + "volatileField.", List(VOLATILE))
  // ACC_SUPER shares its bit with ACC_SYNCHRONIZED, ACC_VARARGS with ACC_TRANSIENT
  checkProperties("class", modifiers, Nil)
  checkProperties("varargs method", "com/javacp/Test#vararg().", Nil)

  test("#1765: annotation type, interface and class") {
    val infos = MetacMetacpDiffExpect.metacpSymbols
    val symbols = List("com/javacp/annot/ClassAnnotation#", "com/javacp/Interface#", modifiers)
    val obtained = symbols.map(sym => sym -> infos(sym).isAnnotationInterface)
    assertEquals(obtained, symbols.zip(List(true, false, false)))
  }

  test("varargs method whose last parameter is not an array") {
    val node = new ClassNode()
    node.access = Opcodes.ACC_PUBLIC
    node.name = "a/Varargs"
    node.superName = "java/lang/Object"
    node.methods
      .add(new MethodNode(Opcodes.ACC_PUBLIC | Opcodes.ACC_VARARGS, "m", "(I)V", null, null))
    def param = Javacp.parse(node, ClasspathIndex(Classpath(Nil))).infos
      .find(_.symbol == "a/Varargs#m().(param0)")
    assertEquals(param.map(_.signature), Some(ValueSignature(TypeRef(NoType, "scala/Int#", Nil))))
  }

}
