package scala.reflect

sealed trait Name
object Name {
  sealed trait Marks
  object NoMarks extends Marks

  final case class TermName(value: String, marks: Marks = NoMarks) extends Name
  final case class TypeName(value: String, marks: Marks = NoMarks) extends Name
}