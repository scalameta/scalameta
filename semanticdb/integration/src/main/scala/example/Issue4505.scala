package example

object Issue4505 {

  import Issue4505J._

  @AnnotationWithEnum(AnnotationEnum.Val1)
  class A

  @AnnotationWithClass(classOf[String])
  class B

}
