package scala.meta
package internal
package trees

import scala.language.experimental.macros
import scala.reflect.macros.blackbox.Context
import scala.annotation.implicitNotFound
import scala.reflect.ClassTag
import org.scalameta.internal.MacroHelpers
import scala.meta.internal.trees.Metadata.Ast
import scala.meta._

@implicitNotFound(msg = "Not found implicit for ${T}, it needs to be added to and imported from scala.meta.internal.trees.AstInfos")
trait AstInfo[T <: Ast] extends ClassTag[T] {
  def runtimeClass: Class[T]
  def quasi(rank: Int, tree: Tree): T with Quasi
}

object AstInfos {

  implicit val implType: AstInfo[Type] = {
    new AstInfo[Type] {
      def runtimeClass: Class[Type] = classOf[Type]
      def quasi(rank: Int, tree: Tree): Type with Quasi = Type.Quasi(rank, tree)
    }
  }
  implicit val implStat: AstInfo[Stat] = {
    new AstInfo[Stat] {
      def runtimeClass: Class[Stat] = classOf[Stat]
      def quasi(rank: Int, tree: Tree): Stat with Quasi = Stat.Quasi(rank, tree)
    }
  }
  implicit val implMod: AstInfo[Mod] = {
    new AstInfo[Mod] {
      def runtimeClass: Class[Mod] = classOf[Mod]
      def quasi(rank: Int, tree: Tree): Mod with Quasi = Mod.Quasi(rank, tree)
    }
  }
  implicit val implTemplate: AstInfo[Template] = {
    new AstInfo[Template] {
      def runtimeClass: Class[Template] = classOf[Template]
      def quasi(rank: Int, tree: Tree): Template with Quasi = Template.Quasi(rank, tree)
    }
  }
  implicit val implModAnnot: AstInfo[Mod.Annot] = {
    new AstInfo[Mod.Annot] {
      def runtimeClass: Class[Mod.Annot] = classOf[Mod.Annot]
      def quasi(rank: Int, tree: Tree): Mod.Annot with Quasi = Mod.Annot.Quasi(rank, tree)
    }
  }
  implicit val implTypeFuncParamClause: AstInfo[Type.FuncParamClause] = {
    new AstInfo[Type.FuncParamClause] {
      def runtimeClass: Class[Type.FuncParamClause] = classOf[Type.FuncParamClause]
      def quasi(rank: Int, tree: Tree): Type.FuncParamClause with Quasi =
        Type.FuncParamClause.Quasi(rank, tree)
    }
  }
  implicit val implTypeArgClause: AstInfo[Type.ArgClause] = {
    new AstInfo[Type.ArgClause] {
      def runtimeClass: Class[Type.ArgClause] = classOf[Type.ArgClause]
      def quasi(rank: Int, tree: Tree): Type.ArgClause with Quasi =
        Type.ArgClause.Quasi(rank, tree)
    }
  }
  implicit val implTermParamClause: AstInfo[Term.ParamClause] = {
    new AstInfo[Term.ParamClause] {
      def runtimeClass: Class[Term.ParamClause] = classOf[Term.ParamClause]
      def quasi(rank: Int, tree: Tree): Term.ParamClause with Quasi =
        Term.ParamClause.Quasi(rank, tree)
    }
  }
  implicit val implTypeParamClause: AstInfo[Type.ParamClause] = {
    new AstInfo[Type.ParamClause] {
      def runtimeClass: Class[Type.ParamClause] = classOf[Type.ParamClause]
      def quasi(rank: Int, tree: Tree): Type.ParamClause with Quasi =
        Type.ParamClause.Quasi(rank, tree)
    }
  }
  implicit val implTermArgClause: AstInfo[Term.ArgClause] = {
    new AstInfo[Term.ArgClause] {
      def runtimeClass: Class[Term.ArgClause] = classOf[Term.ArgClause]
      def quasi(rank: Int, tree: Tree): Term.ArgClause with Quasi =
        Term.ArgClause.Quasi(rank, tree)
    }
  }
  implicit val implArgClause: AstInfo[Member.ArgClause] = {
    new AstInfo[Member.ArgClause] {
      def runtimeClass: Class[Member.ArgClause] = classOf[Member.ArgClause]
      def quasi(rank: Int, tree: Tree): Member.ArgClause with Quasi =
        Member.ArgClause.Quasi(rank, tree)
    }
  }
  implicit val implParamClauseGroup: AstInfo[Member.ParamClauseGroup] = {
    new AstInfo[Member.ParamClauseGroup] {
      def runtimeClass: Class[Member.ParamClauseGroup] = classOf[Member.ParamClauseGroup]
      def quasi(rank: Int, tree: Tree): Member.ParamClauseGroup with Quasi =
        Member.ParamClauseGroup.Quasi(rank, tree)
    }
  }
  implicit val implPatArgClause: AstInfo[Pat.ArgClause] = {
    new AstInfo[Pat.ArgClause] {
      def runtimeClass: Class[Pat.ArgClause] = classOf[Pat.ArgClause]
      def quasi(rank: Int, tree: Tree): Pat.ArgClause with Quasi =
        Pat.ArgClause.Quasi(rank, tree)
    }
  }
  implicit val implTerm: AstInfo[Term] =
    new AstInfo[Term] {
      def runtimeClass: Class[Term] = classOf[Term]
      def quasi(rank: Int, tree: Tree): Term with Quasi = Term.Quasi(rank, tree)
    }

  implicit val implPat: AstInfo[Pat] =
    new AstInfo[Pat] {
      def runtimeClass: Class[Pat] = classOf[Pat]
      def quasi(rank: Int, tree: Tree): Pat with Quasi = Pat.Quasi(rank, tree)
    }
  implicit val implTermRef: AstInfo[Term.Ref] = new AstInfo[Term.Ref] {
    def runtimeClass: Class[Term.Ref] = classOf[Term.Ref]
    def quasi(rank: Int, tree: Tree): Term.Ref with Quasi = Term.Ref.Quasi(rank, tree)
  }
  implicit val implTypeName: AstInfo[Type.Name] =
    new AstInfo[Type.Name] {
      def runtimeClass: Class[Type.Name] = classOf[Type.Name]
      def quasi(rank: Int, tree: Tree): Type.Name with Quasi = Type.Name.Quasi(rank, tree)
    }
  implicit val implTermName: AstInfo[Term.Name] = new AstInfo[Term.Name] {
    def runtimeClass: Class[Term.Name] = classOf[Term.Name]
    def quasi(rank: Int, tree: Tree): Term.Name with Quasi = Term.Name.Quasi(rank, tree)
  }
  implicit val implName: AstInfo[Name] = new AstInfo[Name] {
    def runtimeClass: Class[Name] = classOf[Name]
    def quasi(rank: Int, tree: Tree): Name with Quasi = Name.Quasi(rank, tree)
  }
  implicit val implNameIndeterminate: AstInfo[Name.Indeterminate] =
    new AstInfo[Name.Indeterminate] {
      def runtimeClass: Class[Name.Indeterminate] = classOf[Name.Indeterminate]
      def quasi(rank: Int, tree: Tree): Name.Indeterminate with Quasi =
        Name.Indeterminate.Quasi(rank, tree)
    }

  implicit val implTermParam: AstInfo[Term.Param] =
    new AstInfo[Term.Param] {
      def runtimeClass: Class[Term.Param] = classOf[Term.Param]
      def quasi(rank: Int, tree: Tree): Term.Param with Quasi = Term.Param.Quasi(rank, tree)
    }
  implicit val implTypeParam: AstInfo[Type.Param] =
    new AstInfo[Type.Param] {
      def runtimeClass: Class[Type.Param] = classOf[Type.Param]
      def quasi(rank: Int, tree: Tree): Type.Param with Quasi = Type.Param.Quasi(rank, tree)
    }
  implicit val implSelfQuasi: AstInfo[Self.Quasi] =
    new AstInfo[Self.Quasi] {
      def runtimeClass: Class[Self.Quasi] = classOf[Self.Quasi]
      def quasi(rank: Int, tree: Tree): Self.Quasi with Quasi = Self.Quasi(rank, tree)
    }
  implicit val implNameQuasi: AstInfo[Name.Quasi] =
    new AstInfo[Name.Quasi] {
      def runtimeClass: Class[Name.Quasi] = classOf[Name.Quasi]
      def quasi(rank: Int, tree: Tree): Name.Quasi with Quasi = Name.Quasi(rank, tree)
    }
  implicit val implInit: AstInfo[Init] =
    new AstInfo[Init] {
      def runtimeClass: Class[Init] = classOf[Init]
      def quasi(rank: Int, tree: Tree): Init with Quasi = Init.Quasi(rank, tree)
    }
  implicit val implImportee: AstInfo[Importee] =
    new AstInfo[Importee] {
      def runtimeClass: Class[Importee] = classOf[Importee]
      def quasi(rank: Int, tree: Tree): Importee with Quasi = Importee.Quasi(rank, tree)
    }
  implicit val implImporter: AstInfo[Importer] =
    new AstInfo[Importer] {
      def runtimeClass: Class[Importer] = classOf[Importer]
      def quasi(rank: Int, tree: Tree): Importer with Quasi = Importer.Quasi(rank, tree)
    }
  implicit val implCase: AstInfo[Case] =
    new AstInfo[Case] {
      def runtimeClass: Class[Case] = classOf[Case]
      def quasi(rank: Int, tree: Tree): Case with Quasi = Case.Quasi(rank, tree)
    }
  implicit val implEnumerator: AstInfo[Enumerator] =
    new AstInfo[Enumerator] {
      def runtimeClass: Class[Enumerator] = classOf[Enumerator]
      def quasi(rank: Int, tree: Tree): Enumerator with Quasi = Enumerator.Quasi(rank, tree)
    }
  implicit val implRef: AstInfo[Ref] =
    new AstInfo[Ref] {
      def runtimeClass: Class[Ref] = classOf[Ref]
      def quasi(rank: Int, tree: Tree): Ref with Quasi = Ref.Quasi(rank, tree)
    }
}
