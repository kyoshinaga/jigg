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

