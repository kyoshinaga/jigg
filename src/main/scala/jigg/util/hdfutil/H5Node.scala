package jigg.util.hdfutil

import sun.text.normalizer.Utility
import scala.math.Ordering.Implicits._

/**
  * Created by kenta-yoshinaga on 2016/10/28.
  */
object H5Node{

  def unapplySeq(n: H5Node) = Some((n.label, n.child))

}

abstract class H5Node extends H5NodeSeq {

  def label: String

  def dataType: String

  def child: Seq[H5Node]

  def hasChild: Boolean = child.nonEmpty

  def nonEmptyChildren: Seq[H5Node] = child filterNot (_.toString == "")

  def descendant: List[H5Node] =
    child.toList.flatMap { x => x :: x.descendant }

  def descendantOrSelf: List[H5Node] = this :: descendant

  override def canEqual(other: Any) = other match {
    case x: H5Group => false
    case x: H5Node  => true
    case _          => false
  }

  override protected def basisForHashCode: Seq[Any] = label :: dataType :: nonEmptyChildren.toList

  override def strict_==(other: H5Equality) = other match {
    case _: H5Group => false
    case x: H5Node =>
      (label == x.label) &&
        (dataType == x.dataType) &&
        nonEmptyChildren == x.nonEmptyChildren
    case _ =>
      false
  }

  def theSeq: Seq[H5Node] = this :: Nil

  override def toString(): String = this.label

  override def text: String = super.text

  def data: Seq[Any]
  def ndim: Int
  def dims: Seq[_ <: Long]

  def apply(y: Int, x: Int) = {
    if (data != Nil && ndim == 2 && (y,x) >= (0,0) && y < dims.head && x < dims(1))
      data(y * dims(1).toInt + x)
    else
      throw new IllegalArgumentException("cannot access to data of " + getClass.getSimpleName + " with invalid arguments.")
  }

  def apply(z: Int, y: Int, x: Int) = {
    if (data != Nil && ndim == 3 && (z,y,x) >= (0,0,0) && z < dims.head && y < dims(1) && x < dims(2))
      data((z * dims(1).toInt + y) * dims(2).toInt + x)
    else
      throw new IllegalArgumentException("cannot access to data of " + getClass.getSimpleName + " with invalid arguments.")
  }

  def apply(i: Int, z: Int, y: Int, x: Int) = {
    if (data != Nil && ndim == 4 && i < dims.head && (i,z,y,x) >= (0,0,0,0) && z < dims(1) && y < dims(2) && x < dims(3))
      data(((i * dims(1).toInt + z) * dims(2).toInt + y) * dims(3).toInt + x)
    else
      throw new IllegalArgumentException("cannot access to data of " + getClass.getSimpleName + " with invalid arguments.")
  }

}
