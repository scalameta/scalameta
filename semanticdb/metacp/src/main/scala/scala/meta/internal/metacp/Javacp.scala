package scala.meta.internal.metacp

import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.util.Locale.LanguageRange
import java.util.NoSuchElementException
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.ListBuffer
import scala.meta.internal.semanticdb3.SymbolInformation.{Kind => k}
import scala.meta.internal.semanticdb3.SymbolInformation.{Property => p}
import scala.meta.internal.{semanticdb3 => s}
import scala.tools.asm.ClassReader
import scala.tools.asm.signature.SignatureReader
import scala.tools.asm.signature.SignatureVisitor
import scala.tools.asm.tree.ClassNode
import scala.tools.asm.tree.FieldNode
import scala.tools.asm.tree.MethodNode
import scala.tools.asm.{Opcodes => o}
import org.langmeta.internal.io.PathIO
import org.langmeta.io.AbsolutePath

object Javacp {

  def array(tpe: s.Type): s.Type =
    ref("_root_.scala.Array#", tpe :: Nil)

  def ref(symbol: String, args: List[s.Type] = Nil): s.Type = {
    s.Type(
      s.Type.Tag.TYPE_REF,
      typeRef = Some(s.TypeRef(prefix = None, symbol, args))
    )
  }

  def enclosingPackage(name: String): String = {
    val slash = name.lastIndexOf('/')
    if (slash < 0) sys.error(name)
    else name.substring(0, slash)
  }

  def main(args: Array[String]): Unit = {
    run(args)
    //    val signature =
    //      "<T:Ljava/lang/Object;>(Ljava/util/ArrayList<Ljava/util/ArrayList<[TT;>;>;)Ljava/util/ArrayList<Ljava/util/ArrayList<[TT;>;>;"
    //    val sr = new SignatureReader(signature)
    //    val v = new SemanticdbSignatureVisitor
    //    sr.accept(v)
  }

  def run(args: Array[String]): Unit = {
    val root = AbsolutePath("core/target/scala-2.12/classes/test")
    val scopes = new Scopes()
    Files.walkFileTree(
      root.toNIO,
      new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          if (PathIO.extension(file) == "class") {

            val db = process(root.toNIO, file, scopes)
            if (!file.toString.contains('$')) {
              // pprint.log(db.toProtoString, height = 1000)
              // Main.pprint(db)
            }
          }
          FileVisitResult.CONTINUE
        }
      }
    )

  }

  def process(root: Path, file: Path, scopes: Scopes): s.TextDocument = {
    val bytes = Files.readAllBytes(file)
    val node = asmNodeFromBytes(bytes)
    val symbols = process(node, scopes)
    val uri = root.relativize(file).toString
    s.TextDocument(
      schema = s.Schema.SEMANTICDB3,
      uri = uri,
      symbols = symbols
    )
  }

  def process(node: ClassNode, scopes: Scopes): Seq[s.SymbolInformation] = {

    val buf = ArrayBuffer.empty[s.SymbolInformation]

    val classSymbol = ssym(node.name)
    val className = getName(node.name)
    val isTopLevelClass = !node.name.contains("$")

    def addPackage(name: String, owner: String): String = {
      val packageSymbol = owner + name + "."
      buf += s.SymbolInformation(
        symbol = packageSymbol,
        kind = k.PACKAGE,
        name = name,
        owner = owner
      )
      packageSymbol
    }
    val classOwner: String = if (isTopLevelClass) {
      // Emit packages
      val packages = node.name.split("/")
      packages.iterator
        .take(packages.length - 1)
        .foldLeft(addPackage("_root_", "")) {
          case (owner, name) => addPackage(name, owner)
        }
    } else {
      ssym(node.name.substring(0, node.name.length - className.length - 1))
    }

    val classKind =
      if (node.access.hasFlag(o.ACC_INTERFACE)) k.TRAIT
      else k.CLASS

    val classDeclaration =
      if (node.signature == null) None
      else {
        // TODO
        None
      }

    val decls = List.empty[String]

    val classTpe = s.Type(
      tag = s.Type.Tag.CLASS_INFO_TYPE,
      classInfoType = Some(
        s.ClassInfoType(
          typeParameters = Nil,
          parents = Nil,
          declarations = decls
        )
      )
    )

    buf += s.SymbolInformation(
      symbol = classSymbol,
      kind = classKind,
      name = className,
      owner = classOwner,
      tpe = Some(classTpe),
      accessibility = saccessibility(node.access, classOwner)
    )

    buf.result()
  }

  def asmNodeFromBytes(bytes: Array[Byte]): ClassNode = {
    val node = new ClassNode()
    new ClassReader(bytes).accept(
      node,
      ClassReader.SKIP_DEBUG |
        ClassReader.SKIP_FRAMES |
        ClassReader.SKIP_CODE
    )
    node
  }

  def getName(symbol: String): String = {
    val dollar = symbol.lastIndexOf('$')
    if (dollar < 0) {
      val slash = symbol.lastIndexOf('/')
      if (slash < 0) sys.error(s"Missing $$ or / from symbol '$symbol'")
      else symbol.substring(slash + 1)
    } else {
      symbol.substring(dollar + 1)
    }
  }

  def ssym(string: String): String =
    "_root_." + string.replace('$', '#').replace('/', '.') + "#"

  implicit class XtensionAccess(n: Int) {
    def hasFlag(flag: Int): Boolean =
      (flag & n) != 0
  }

  def accessibility(tag: s.Accessibility.Tag): Option[s.Accessibility] = Some(s.Accessibility(tag))
  def saccessibility(access: Int, owner: String): Option[s.Accessibility] = {
    val a = s.Accessibility.Tag
    if (access.hasFlag(o.ACC_PUBLIC)) accessibility(a.PUBLIC)
    else if (access.hasFlag(o.ACC_PROTECTED)) accessibility(a.PROTECTED)
    else if (access.hasFlag(o.ACC_PRIVATE)) accessibility(a.PRIVATE)
    else {
      Some(
        s.Accessibility(a.PRIVATE_WITHIN, owner.substring(0, owner.lastIndexOf('.')))
      )
    }
  }

}
