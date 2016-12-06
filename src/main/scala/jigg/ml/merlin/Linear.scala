package jigg.ml.merlin

import breeze.linalg.{DenseMatrix, DenseVector}
import jigg.util.hdfutil.H5Node

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

class Linear private(indim: Int, outdim: Int) extends Functor{
  override def functorName: String = "Linear"

  private val w = DenseMatrix.zeros[Float](indim, outdim)

  private val b = DenseVector.zeros[Float](outdim)

  protected def h5load(W: H5Node, B: H5Node):Unit ={
    for(y <- 0 until W.dims.head.toInt)
      for(x <- 0 until W.dims(1).toInt) {
        w(y, x) = W(y, x).asInstanceOf[Float]
        if (y == 0){
          b(x) = B.data(x).asInstanceOf[Float]
        }
      }
  }

  override final def convert(x:DenseMatrix[Float]) = {
    val z = w.t * x
    for (i <- 0 until x.cols){
      z(::,i) := z(::,i) + b
    }
    z
  }
}

object Linear {

  def apply(indim:Int, outdim:Int) = new Linear(indim, outdim)

  def apply(h5node: H5Node): Linear = {
    val b = h5node.child(1)
    val w = h5node.child(2)
    val ls = new Linear(w.dims.head.toInt, w.dims(1).toInt)
    ls.h5load(w, b)
    ls
  }

}