package scala.meta

private[meta] trait VersionSpecificApis extends transversers.Api with quasiquotes.Api {

  // transversers

  type Transformer = transversers.Transformer
  type Traverser = transversers.Traverser

}
