package jigg.util.hdfutil

/**
  * Created by kenta-yoshinaga on 2016/10/25.
  */
object H5Elem {

  def apply(label: String, dataType: String, child: H5Node*): H5Elem =
    apply(label, dataType, child.isEmpty, child: _*)

  def apply(label: String, dataType: String, minimizeEmpty: Boolean, child: H5Node*): H5Elem =
    new H5Elem(label, dataType, minimizeEmpty, child: _*)

  def unapplySeq(n: H5Node) = n match {
    case _: H5Group => None
    case _          => Some((n.label, n.dataType, n.child))
  }

}

class H5Elem(val label: String,
             val dataType: String,
             val minimizeEmpty: Boolean,
             val child: H5Node*) extends H5Node with Serializable {

  def this(label: String, dataType: String, child: H5Node*) = {
    this(label, dataType, child.isEmpty, child: _*)
  }

  override def basisForHashCode: Seq[Any] = label :: dataType :: child.toList

  override def text: String = (child map ( _.text)).mkString

  override def data = Nil

  override def ndim: Int = -1

  override def dims: Seq[_ <: Long] = Seq(data.length)

  def copy(label: String = this.label,
           dataType: String = this.dataType,
           minimizeEmpty: Boolean = this.minimizeEmpty,
           child: Seq[H5Node] = this.child): H5Elem =
    H5Elem(label,dataType,minimizeEmpty,child: _*)

}

