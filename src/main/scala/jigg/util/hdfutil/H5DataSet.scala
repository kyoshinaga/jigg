package jigg.util.hdfutil

/*
 Copyright 2013-2016 Kenta Yoshinaga

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

object H5DataSet {
  def apply[T](data: Seq[T], label: String): H5DataSet[T] =
    new H5DataSet[T](data, label, 1, Seq(data.length))

  def apply[T](data: Seq[T], label: String, ndim: Int, dims: Seq[_ <: Long]): H5DataSet[T] =
    new H5DataSet[T](data, label, ndim, dims)

  def unapplySeq[T](n: H5DataSet[T]) = Some((n.data, n.label, n.ndim, n.dims))
}

class H5DataSet[+A] (val data: Seq[A],
                     val label: String,
                     val ndim: Int,
                     val dims: Seq[_ <: Long]) extends H5Node with Serializable {

  if( data == null)
    throw new IllegalArgumentException("cannot construct " + getClass.getSimpleName + " with null")

  if( dims.product != data.length )
    throw new IllegalArgumentException("cannot construct " + label +
      "[" + getClass.getSimpleName + "]" + " with invalid dims\n" +
      "product of dims: " + dims.product + "\n" +
      "data length: " + data.length)

  if( ndim != dims.length )
    throw new IllegalArgumentException("cannot construct " + getClass.getSimpleName + " with unmatched ndim ")

  override protected def basisForHashCode: Seq[Any] = Seq(data)

  override def strict_==(other: H5Equality) = other match {
    case x:H5DataSet[_] => data == x.data
    case _              => false
  }

  override def canEqual(other: Any) = other match {
    case _:H5DataSet[_] => true
    case _              => false
  }

  override def dataType: String = "DataSet"

  override def child: Seq[H5Node] = Nil

  override def text: String = data.toString

}
