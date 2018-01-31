package scala.meta.internal.semanticdb.scalac

import java.io.Writer

// A writer that keeps track of the current length.
class LengthWriter(delegate: Writer, start: Int) extends Writer {
  var length: Int = start
  override def flush(): Unit = {
    delegate.flush()
  }
  override def write(cbuf: Array[Char], off: Int, len: Int): Unit = {
    length += len
    delegate.write(cbuf, off, len)
  }
  override def close(): Unit = {
    delegate.close()
  }
}
