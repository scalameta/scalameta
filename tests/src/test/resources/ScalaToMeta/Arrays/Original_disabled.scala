object Arrays {
  def empty[T: scala.reflect.ClassTag]: Array[T] = new Array[T](0)
}