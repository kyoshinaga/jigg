package jigg.util.hdfutil

/**
  * Created by kenta-yoshinaga on 2016/10/28.
  */
final case class H5Group(nodes: Seq[H5Node]) extends H5Node {
  override def theSeq = nodes

  override def canEqual(other: Any) = other match {
    case x: H5Group => true
    case _          => false
  }

  override def strict_==(other: H5Equality) = other match {
    case H5Group(xs)  => nodes sameElements xs
    case _          => false
  }

  override protected def basisForHashCode = nodes

  private def fail(msg: String) = throw new UnsupportedOperationException("class H5Group does not support method '%s'" format msg)

  def label = fail("label")
  override def child = fail("child")
  override def dataType = fail("datatype")
  override def data = fail("data")
  override def ndim = fail("ndim")
  override def dims = fail("dims")
}
