package example

object Issue4505/*<=example.Issue4505.*/ {

  import Issue4505J/*=>example.Issue4505J#*/._

  @AnnotationWithEnum/*=>example.Issue4505J#AnnotationWithEnum#*/(AnnotationEnum.Val1)
  class A/*<=example.Issue4505.A#*/

  @AnnotationWithClass/*=>example.Issue4505J#AnnotationWithClass#*/(classOf[String])
  class B/*<=example.Issue4505.B#*/

}
