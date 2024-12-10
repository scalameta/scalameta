package scala.meta

private[meta] enum QuasiquoteType:
  case Term // q""
  case TermParam // param""
  case Type // t""
  case TypeParam // tparam""
  case CaseOrPattern // p""
  case Init // init""
  case Self //self""
  case Template // template""
  case Mod // mod""
  case Enumerator // enumerator""
  case Importer // importer""
  case Importee // importee""
  case Source // source""

  def parserClass() =
    "scala.meta.quasiquotes.Api.XTensionQuasiquote" + toString
