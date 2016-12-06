package jigg.util.hdfutil

/*
 Copyright 2013-2015 Kenta Yoshinaga

 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

import scala.collection.{mutable, immutable, AbstractSeq, generic, SeqLike}
import mutable.{Builder, ListBuffer}
import generic.{CanBuildFrom}
import scala.language.implicitConversions

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
