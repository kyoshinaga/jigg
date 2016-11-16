package jigg.util.hdfutil

import scala.collection.{mutable, immutable, AbstractSeq, generic, SeqLike}
import mutable.{Builder, ListBuffer}
import generic.{CanBuildFrom}
import scala.language.implicitConversions

/**
  * Created by kenta-yoshinaga on 2016/10/28.
  */
object H5NodeSeq {
  final val Empty = fromSeq(Nil)
  def fromSeq(s: Seq[H5Node]): H5NodeSeq = new H5NodeSeq{
    def theSeq = s
  }
  type Coll = H5NodeSeq
  implicit def canBuildFrom: CanBuildFrom[Coll, H5Node, H5NodeSeq] =
    new CanBuildFrom[Coll, H5Node, H5NodeSeq] {
      def apply(from: Coll) = newBuilder
      def apply() = newBuilder
    }

  def newBuilder: Builder[H5Node, H5NodeSeq] = new ListBuffer[H5Node] mapResult fromSeq
  implicit def seqToNodeSeq(s: Seq[H5Node]): H5NodeSeq = fromSeq(s)

}
abstract class H5NodeSeq extends AbstractSeq[H5Node]
  with immutable.Seq[H5Node] with SeqLike[H5Node, H5NodeSeq]
  with H5Equality {

  import H5NodeSeq.seqToNodeSeq

  override protected[this] def newBuilder = H5NodeSeq.newBuilder

  def theSeq: Seq[H5Node]
  def length = theSeq.length
  override def iterator = theSeq.iterator

  def apply(i: Int): H5Node = theSeq(i)
  def apply(f: H5Node => Boolean): H5NodeSeq = filter(f)

  def h5SameElements[A](that: Iterable[A]): Boolean = {
    val these = this.iterator
    val those = that.iterator
    while(these.hasNext && those.hasNext)
      if(these.next h5_!= those.next)
        return false

    !these.hasNext && !those.hasNext
  }

  protected def basisForHashCode: Seq[Any] = theSeq

  override def canEqual(other: Any) = other match {
    case _: H5NodeSeq => true
    case _            => false
  }

  override def strict_==(other: H5Equality) = other match {
    case x: H5NodeSeq => (length == x.length) && (theSeq sameElements x.theSeq)
    case _            => false
  }

  override def toString(): String = theSeq.mkString

  def text: String = (this map(_.text)).mkString

}
