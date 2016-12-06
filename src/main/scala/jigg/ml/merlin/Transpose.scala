package jigg.ml.merlin
import breeze.linalg.DenseMatrix

/*
 Copyright 2013-2015 Hiroshi Noji

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

object Transpose extends Functor{

  override def functorName: String = "Transpose"

  override def convert(data: DenseMatrix[Float]): DenseMatrix[Float] = data.t

  def apply(m: DenseMatrix[Float]) = this.convert(m)

}
