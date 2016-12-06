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
