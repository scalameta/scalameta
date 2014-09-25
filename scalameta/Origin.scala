package scala.meta
import org.scalameta.adt._

@root trait Origin { def src: Source }
object Origin {
  @leaf object None extends Origin { def src = meta.Source.None }
  @leaf class Source(src: meta.Source) extends Origin // TODO: positions
  @leaf class Transform(proto: Tree, origin: Origin) extends Origin { def src = origin.src }
  implicit def defaultOriginIsNone: Origin = None
}