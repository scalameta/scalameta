package scala.meta.internal.semanticdb.javac.semantics

import com.github.{javaparser => jp}
import javax.tools.JavaFileObject
import scala.annotation.switch
import scala.collection.JavaConverters._
import scala.collection.mutable
import scala.compat.java8.OptionConverters._
import scala.meta.internal.semanticdb.Scala.{Descriptor => d, _}

trait Semantics extends Elements with TypeMirrors {
  type SymbolTable = mutable.Map[String, Option[jp.Range]]
  def emptySymbolTable: SymbolTable =
    mutable.Map.empty[String, Option[jp.Range]]

  class SymbolTableGenerator extends jp.ast.visitor.VoidVisitorAdapter[SymbolTable] {
    override def visit(n: jp.ast.PackageDeclaration, arg: SymbolTable): Unit = {
      super.visit(n, arg)
      val range = n.getName.getRange
      arg += n.sym -> range.asScala
    }

    override def visit(vd: jp.ast.body.VariableDeclarator, arg: SymbolTable): Unit = {
      super.visit(vd, arg)
      val range = vd.getName.getRange
      arg += vd.sym -> range.asScala
    }

    override def visit(cid: jp.ast.body.ClassOrInterfaceDeclaration, arg: SymbolTable): Unit = {
      super.visit(cid, arg)
      val range = cid.getName.getRange
      arg += cid.sym -> range.asScala
    }

    override def visit(p: jp.ast.body.Parameter, arg: SymbolTable): Unit = {
      super.visit(p, arg)
      val range = p.getName.getRange
      arg += p.sym -> range.asScala
    }

    override def visit(md: jp.ast.body.MethodDeclaration, arg: SymbolTable): Unit = {
      super.visit(md, arg)
      val range = md.getName.getRange
      arg += md.sym -> range.asScala
    }

    override def visit(ad: jp.ast.body.AnnotationDeclaration, arg: SymbolTable): Unit = {
      super.visit(ad, arg)
      val range = ad.getName.getRange
      arg += ad.sym -> range.asScala
    }

    override def visit(tp: jp.ast.`type`.TypeParameter, arg: SymbolTable): Unit = {
      super.visit(tp, arg)
      val range = tp.getName.getRange
      arg += tp.sym -> range.asScala
    }

    override def visit(ecd: jp.ast.body.EnumConstantDeclaration, arg: SymbolTable): Unit = {
      super.visit(ecd, arg)
      val range = ecd.getName.getRange
      arg += ecd.sym -> range.asScala
    }

    override def visit(ed: jp.ast.body.EnumDeclaration, arg: SymbolTable): Unit = {
      super.visit(ed, arg)

      // add synthetic `values` method
      val valuesMethodName = "values"
      if (ed.getMethodsByName(valuesMethodName).isEmpty) {
        val methodSymbol = Symbols.Global(ed.sym, d.Method(valuesMethodName, "()"))
        arg += methodSymbol -> None
      }

      // add synthetic `valueOf(string)` method
      val valueOfMethodName = "valueOf"
      if (ed.getMethodsByName(valueOfMethodName).isEmpty) {
        val methodSymbol = Symbols.Global(ed.sym, d.Method(valueOfMethodName, "()"))
        arg += methodSymbol -> None

        val parameterSymbol = Symbols.Global(methodSymbol, d.Parameter("name"))
        arg += parameterSymbol -> None
      }

      val range = ed.getName.getRange
      arg += ed.sym -> range.asScala
    }
  }

  implicit class NodeOps(node: jp.ast.Node) {
    def owner: String = {
      node.getParentNode.asScala match {
        case Some(parent) =>
          parent match {
            case elem: jp.ast.CompilationUnit =>
              elem.getPackageDeclaration.asScala.map { _.sym }.getOrElse("")
            case elem @ (
                  _: jp.ast.body.TypeDeclaration[_] | _: jp.ast.body.CallableDeclaration[_] |
                  _: jp.ast.PackageDeclaration
                ) =>
              elem.sym
            case _ =>
              parent.owner
          }
        case _ =>
          throw new RuntimeException("Unexpected kind of node. Please, submit the issue")
      }
    }

    def symbolName: String = node match {
      case _: jp.ast.body.ConstructorDeclaration =>
        "<init>"
      case elem: jp.ast.nodeTypes.NodeWithSimpleName[_] =>
        elem.getNameAsString
      case _ =>
        throw new RuntimeException("Unexpected kind of node. Please, submit the issue")
    }

    def sym: String = node match {
      case elem: jp.ast.CompilationUnit =>
        elem.getPackageDeclaration.asScala.map { _.sym }.getOrElse("")
      case elem: jp.ast.PackageDeclaration =>
        val qualName = elem.getName.asString
        if (qualName == "") {
          Symbols.EmptyPackage
        } else {
          qualName.replace('.', '/') + "/"
        }
      case _: jp.ast.body.TypeDeclaration[_] =>
        Symbols.Global(owner, d.Type(symbolName))
      case _: jp.ast.stmt.LocalClassDeclarationStmt =>
        ""
      case _: jp.ast.body.Parameter =>
        Symbols.Global(owner, d.Parameter(symbolName))
      case elem: jp.ast.body.CallableDeclaration[_] =>
        elem.getParentNode.asScala match {
          case Some(owner: jp.ast.body.TypeDeclaration[_]) =>
            val disambig = {
              val siblingMethods: mutable.Buffer[jp.ast.body.CallableDeclaration[_]] = {
                if (elem.isConstructorDeclaration) {
                  val siblingMethods = owner
                    .asInstanceOf[jp.ast.nodeTypes.NodeWithConstructors[_]]
                    .getConstructors
                    .asScala
                  siblingMethods.map { _.asCallableDeclaration }
                } else {
                  val siblingMethods = owner.getMethodsByName(symbolName).asScala
                  val (instance, static) = siblingMethods.partition { method =>
                    !method.getModifiers.contains(jp.ast.Modifier.STATIC)
                  }
                  val r = instance ++ static
                  r.map { _.asCallableDeclaration }
                }
              }
              val methodPlace = siblingMethods.indexOf(elem)
              (methodPlace: @switch) match {
                case -1 =>
                  throw new RuntimeException("Unexpected state. Please, submit the issue")
                case 0 => "()"
                case x => s"(+$x)"
              }
            }
            Symbols.Global(owner.sym, d.Method(symbolName, disambig))
          case Some(_: jp.ast.expr.ObjectCreationExpr) =>
            ""
          case _ =>
            throw new RuntimeException("Unexpected kind of node. Please, submit the issue")
        }
      case _: jp.ast.body.EnumConstantDeclaration =>
        Symbols.Global(owner, d.Term(symbolName))
      case _: jp.ast.body.VariableDeclarator =>
        val sn = symbolName
        Symbols.Global(owner, d.Term(sn))
      case _: jp.ast.`type`.TypeParameter =>
        Symbols.Global(owner, d.TypeParameter(symbolName))
      case _ =>
        throw new RuntimeException("Unexpected kind of node. Please, submit the issue")
    }
  }

  val sourceFile: JavaFileObject

  val symbolTable: SymbolTable = {
    val content = sourceFile.getCharContent(true).toString
    val cu = jp.JavaParser.parse(content)
    val stg = new SymbolTableGenerator()
    val symbolTable = emptySymbolTable
    stg.visit(cu, symbolTable)
    symbolTable
  }
}
